package is.clipperz.backend.storage

import is.clipperz.backend.Exceptions.{ EmptyContentException, NonReadableArchiveException, NonWritableArchiveException, ResourceNotFoundException }
import is.clipperz.backend.otel.TracingAspect.MethodTracer

import java.io.FileOutputStream

import zio.{ Duration, Task, ZIO }
import zio.nio.file.{ Files, Path }
import zio.stream.{ ZSink, ZStream }
import zio.telemetry.opentelemetry.tracing.Tracing
import zio.metrics.MetricLabel

case class FileSystemKeyValueStorage private (basePath: Path, levels: Int) extends KeyValueStorage:

        protected def archive: String = basePath.filename.toString()

        protected def storageType: String = "fileSystem"

        private def getContent (key: Key, contentType: ContentType): ZIO[Tracing, Throwable, (ZStream[Any, Throwable, Byte], Long)] =
            getBlobPath(key, false)
            .map(path => pathForContentType(key, path, contentType))
            .flatMap(path =>
                Files.exists(path)
                .flatMap(exists => exists match {
                        case true   => (Files.readAllBytes(path).map(ZStream.fromChunk)).zip(Files.size(path))
                        case false  => ZIO.fail(new ResourceNotFoundException("Referenced blob does not exists"))
                })
            )

        override def getBlob (key: Key): ZIO[Tracing, Throwable, (ZStream[Any, Throwable, Byte], Long)] = getContent(key, ContentType.Blob) @@ MethodTracer("getBlobContent")

        override def getMetadata(key: Key): ZIO[Tracing, Throwable, ZStream[Any, Throwable, Byte]] = getContent(key, ContentType.Metadata).map(_._1) @@ MethodTracer("getMetadata")

        private def saveData (key: Key, contentType: ContentType, content: ZStream[Any, Throwable, Byte], overwrite: Boolean): ZIO[Tracing, Throwable, Unit] = 
            getBlobPath(key, true)
            .map(path => pathForContentType(key, path, contentType))
            .mapError(_ => new NonWritableArchiveException("Could not create blob file"))
            .flatMap(path => content
                .timeoutFail(new EmptyContentException)(Duration.fromMillis(WAIT_TIME))
                .run(ZSink.fromOutputStream(new FileOutputStream(path.toFile)))
                .map(_ => ())
            )
            .catchSome:
                case ex: EmptyContentException => ZIO.fail(ex)
                case ex: NonReadableArchiveException => ZIO.fail(ex)
                case ex => ZIO.fail(new NonWritableArchiveException(s"${ex}"))

        private def moveFile (key: Key, content: Path): ZIO[Tracing, Throwable, Unit] =
            getBlobPath(key, true)
            .map(path => pathForContentType(key, path, ContentType.Blob))
            .flatMap(path => Files.move(content, path))

        private def saveMetadata (key: Key, metadata: ZStream[Any, Throwable, Byte], overwrite: Boolean): ZIO[Tracing, Throwable, Unit] = saveData(key, ContentType.Metadata, metadata, overwrite) @@ MethodTracer("saveMetadata")

        override def saveBlob (key: Key, content:  ZStream[Any, Throwable, Byte], overwrite: Boolean): ZIO[Tracing, Throwable, Unit] = saveData(key, ContentType.Blob,     content, overwrite) @@ MethodTracer("saveBlob")

        override def saveBlobWithMetadata (key: Key, content: ZStream[Any, Throwable, Byte], metadata: ZStream[Any, Throwable, Byte], overwrite: Boolean): ZIO[Tracing, Throwable, Unit] =
            saveBlob(key, content, overwrite) <&> saveMetadata(key, metadata, overwrite) 
            @@ MethodTracer("saveBlobWithMetadata")

        override def deleteBlob (key: Key): ZIO[Tracing, Throwable, Unit] =
            getBlobPath(key, false)
                .flatMap(path =>
                    Files.deleteIfExists(pathForContentType(key, path, ContentType.Blob))
                    <&>
                    Files.deleteIfExists(pathForContentType(key, path, ContentType.Metadata))
                )
            .foldZIO(err => ZIO.fail(new NonWritableArchiveException(err.toString())), _ => ZIO.succeed(()))
            @@ MethodTracer("deleteBlob")

        private def computeBlobPath (key: Key): Path =
            val piecesLength: Int = key.length / levels
            val pieces: IndexedSeq[String] =
                for (i <- 0 to levels - 1)
                yield key.substring(i * piecesLength, i * piecesLength + piecesLength).nn
            basePath / pieces.mkString("/")

        private def getBlobPath (key: Key, createFolders: Boolean): ZIO[Tracing, Throwable, Path] =
            val path: Path = computeBlobPath(key)
            Files.exists(path)
            .zip(Files.isDirectory(path))
            .flatMap((exists, isDirectory) => (exists, isDirectory, createFolders) match {
                case (true,  true,  _    ) => ZIO.succeed(path)
                case (true,  false, _    ) => ZIO.fail(new ResourceNotFoundException(s"Referenced blob does not exists"))
                case (false, _,     false) => ZIO.fail(new ResourceNotFoundException(s"Referenced blob does not exists"))
                case (false, _,     true ) => Files.createDirectories(path) *> ZIO.succeed(path)
            })

        protected def measure: Task[Map[String, Long]] =
            for {
                (count, space)  <-  elapsedTime("files", Set(MetricLabel("archive", archive)))(
                                        Files.walk(basePath)
                                        .filterZIO(path => Files.isRegularFile(path).zip(Files.isHidden(path)).map((regular, hidden) => (regular && !hidden)))
                                        .mapZIO(path => Files.size(path).map(size => (1, size)))
                                        .runFold((0L, 0L))((acc, tuple) => ((acc._1 + tuple._1), (acc._2 + tuple._2)))
                                    )
            } yield(Map("files.count" -> count, "files.size" -> space))

object FileSystemKeyValueStorage:
    def apply (
        basePath: Path,
        levels: Int,
        requireExistingPath: Boolean = true,
    ): Task[FileSystemKeyValueStorage] =
        Files.exists(basePath)
        .zip(Files.isDirectory(basePath))
        .flatMap((exists, isDirectory) => {
            ((exists, isDirectory, requireExistingPath) match {
                    case (true,  true,  _    )  => ZIO.succeed(())
                    case (true,  false, false)  => Files.deleteRecursive(basePath) *> Files.createDirectories(basePath) *> ZIO.succeed(())
                    case (false, _,     false)  => Files.createDirectories(basePath) *> ZIO.succeed(())
                    case (true,  false, true )  => ZIO.fail(new Exception(s"base folder file already exists, but is not a folder: ${basePath}"))
                    case (false, _,     true )  => ZIO.fail(new Exception(s"base folder does not exists: ${basePath}"))
            })
            *>  ZIO.succeed(new FileSystemKeyValueStorage(basePath, levels))
            .tap(_.scheduled.forkDaemon)
        })
