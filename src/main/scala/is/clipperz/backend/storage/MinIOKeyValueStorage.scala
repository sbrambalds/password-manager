package is.clipperz.backend.storage

import is.clipperz.backend.Exceptions.{ EmptyContentException, NonReadableArchiveException, NonWritableArchiveException, ResourceNotFoundException }
import is.clipperz.backend.otel.TracingAspect.MethodTracer

import java.io.FileOutputStream

import zio.{ Duration, Task, ZIO }
import zio.nio.file.{ Files, Path }
import zio.stream.{ ZSink, ZStream }
import zio.telemetry.opentelemetry.tracing.Tracing
import zio.s3.S3
import software.amazon.awssdk.services.s3.model.S3Exception
import software.amazon.awssdk.utils.Md5Utils

case class MinIOKeyValueStorage private (bucketName: String, s3: S3) extends KeyValueStorage:

    protected def archive: String = bucketName

    protected def storageType: String = "s3"

    override def getBlob (key: Key): ZIO[Tracing, Throwable, (ZStream[Any, Throwable, Byte], Long)] = 
        s3.getObject(bucketName, computeDataPath(key, ContentType.Blob))
            .runCollect
            .map(chunk => (ZStream.fromChunk(chunk), chunk.size.toLong))
            .mapError(_ => new ResourceNotFoundException("Resource not found"))
        @@ MethodTracer("getBlobContent")

    override def getMetadata(key: Key): ZIO[Tracing, Throwable, ZStream[Any, S3Exception, Byte]] = 
        s3.getObject(bucketName, computeDataPath(key, ContentType.Metadata))
            .runCollect
            .map(ZStream.fromChunk)
        @@ MethodTracer("getBlobMetadata")    

    private def saveData (key: Key, content: ZStream[Any, Throwable, Byte], contentType: ContentType): ZIO[Tracing, Throwable, Unit] =
        (for {
            contentData     <-  content.runCollect.map(_.toArray)
            _               <-  s3.putObject(
                                    bucketName, 
                                    computeDataPath(key, contentType),
                                    contentData.length.toLong,
                                    content,
                                    contentMD5 = Some(Md5Utils.md5AsBase64(contentData))
                                )
        } yield())
        .timeoutFail(new EmptyContentException)(Duration.fromMillis(WAIT_TIME))
        .catchSome:
            case ex: EmptyContentException => ZIO.fail(ex)
            case ex: NonReadableArchiveException => ZIO.fail(ex)
            case ex => ZIO.fail(new NonWritableArchiveException(s"${ex}"))

    override def saveBlob (key: Key, content:  ZStream[Any, Throwable, Byte], overwrite: Boolean): ZIO[Tracing, Throwable, Unit] = 
        saveData(key, content, ContentType.Blob)
        @@ MethodTracer("saveBlob")

    override def saveBlobWithMetadata (key: Key, content: ZStream[Any, Throwable, Byte], metadata: ZStream[Any, Throwable, Byte], overwrite: Boolean): ZIO[Tracing, Throwable, Unit] = 
        saveData(key, content, ContentType.Blob) <*> saveData(key, metadata, ContentType.Metadata)
        @@ MethodTracer("saveBlobWithMetadata")

    override def deleteBlob (key: Key): ZIO[Tracing, Throwable, Unit] = 
        s3.deleteObject(bucketName, computeDataPath(key, ContentType.Blob))
        @@ MethodTracer("deleteBlob")

    private def computeDataPath (key: Key, contentType: ContentType): String = pathForContentType(key, Path(""), contentType).toString

    protected def measure: Task[Map[String, Long]] = 
        val files = s3.listAllObjects(bucketName)
        ZIO.scoped(
            for {
                count           <- files.run(ZSink.count)
                space           <- files.map(_.size).run(ZSink.sum[Long])
            } yield (Map("files.count" -> count, "files.size" -> space))
        )

object MinIOKeyValueStorage:
    def apply(
        bucketName: String
    ): ZIO[S3, Throwable, MinIOKeyValueStorage] = 
        for {
            s3          <- ZIO.service[S3]
            exists      <- s3.isBucketExists(bucketName)
            _           <- ZIO.when(exists)(
                            for {
                                objects <- s3.listAllObjects(bucketName).runCollect
                                _       <- ZIO.foreachDiscard(objects)(obj => s3.deleteObject(bucketName, obj.key))
                                _       <- s3.deleteBucket(bucketName)
                                _       <- s3.createBucket(bucketName)
                            } yield ()
                        )
            _           <-  ZIO.when(!exists)(
                            for {
                                _       <- s3.createBucket(bucketName)
                            } yield ()
                        )
            storage     =   new MinIOKeyValueStorage(bucketName, s3)
            _           <-  storage.scheduled.forkDaemon
        } yield storage
