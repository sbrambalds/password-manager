package is.clipperz.backend.storage

import is.clipperz.backend.sqlite.DbTable
import com.augustnagro.magnum.Repo
import com.augustnagro.magnum.magzio.Transactor
import zio.stream.ZStream
import zio.ZIO
import zio.telemetry.opentelemetry.tracing.Tracing
import java.nio.charset.StandardCharsets
import is.clipperz.backend.Exceptions.NonWritableArchiveException
import is.clipperz.backend.Exceptions.EmptyContentException
import zio.Duration
import is.clipperz.backend.otel.TracingAspect.MethodTracer
import is.clipperz.backend.Exceptions.ResourceNotFoundException
import zio.Task
import is.clipperz.backend.sqlite.name
import is.clipperz.backend.sqlite.createTable

case class SqlLiteKeyValueStorage[T <: DbTable] private (repo: Repo[T, T, Key], transactor: Transactor, factory: (Key, String, Array[Byte]) => T) extends KeyValueStorage : 

        protected def archive: String = repo.name

        protected def storageType: String = "db"
        
        override def saveBlob(key: Key, content: ZStream[Any, Throwable, Byte], overwrite: Boolean): ZIO[Tracing, Throwable, Unit] =
            saveBlobWithMetadata(key, content, ZStream.empty, overwrite)
            @@ MethodTracer("saveBlob")

        override def saveBlobWithMetadata(key: Key, content: ZStream[Any, Throwable, Byte], metadata: ZStream[Any, Throwable, Byte], overwrite: Boolean): ZIO[Tracing, Throwable, Unit] = 
            (for {
                identifier  <- metadata.runCollect
                                    .map(chunck => new String(chunck.toArray, StandardCharsets.UTF_8))
                                    .mapError(_ => new NonWritableArchiveException("Could not create blob file"))
                data        <- content.runCollect.map(_.toArray)
                entity      =  factory(key, identifier, data)
                insertRes   <- transactor.transact {
                        overwrite match {
                            case true  => repo.update(entity)
                            case false => repo.insert(entity)
                        }
                    }    
            } yield ())
            .timeoutFail(new EmptyContentException)(Duration.fromMillis(WAIT_TIME))
            @@ MethodTracer("saveBlobMetadata")

        override def getBlob(key: Key): ZIO[Tracing, Throwable, (ZStream[Any, Throwable, Byte], Long)] =
            (transactor.transact:
                repo.findById(key) match
                    case Some(dbTable)  => (ZStream.fromIterable(dbTable.blob), dbTable.blob.length.toLong)
                    case _              => throw new ResourceNotFoundException("Referenced document does not exist")
            ) @@ MethodTracer("getBlob")
            
        override def getMetadata(key: Key): ZIO[Tracing, Throwable, ZStream[Any, Throwable, Byte]] = 
            (transactor.transact: 
                repo.findById(key) match 
                    case Some(dbTable)      => ZStream.fromIterable(dbTable.content.getBytes())
                    case _                  => throw new ResourceNotFoundException("Referenced document does not exist")
            ) @@ MethodTracer("getMetaData")

        override def deleteBlob(key: Key): ZIO[Tracing, Throwable, Unit] = 
            (transactor.transact:
                repo.deleteById(key)
            .mapError(_ => new ResourceNotFoundException("Referenced document does not exist"))
            ) @@ MethodTracer("deleteBlob")
        
        protected def measure: Task[Map[String, Long]] = 
            transactor.transact:
                val all = repo.findAll
                Map(
                    "files.count" -> all.length.toLong, 
                    "files.size" -> all.map(_.blob.length.toLong).sum
                )

object SqlLiteKeyValueStorage:
    def apply[T <: DbTable](repo: Repo[T, T, Key], factory: (Key, String, Array[Byte]) => T): ZIO[Transactor, Throwable, SqlLiteKeyValueStorage[T]] = 
        for {
            transactor  <- ZIO.service[Transactor]
            _           <- repo.createTable(transactor)
            storage     =  new SqlLiteKeyValueStorage[T](repo, transactor, factory)
            _           <- storage.scheduled.forkDaemon
        } yield storage