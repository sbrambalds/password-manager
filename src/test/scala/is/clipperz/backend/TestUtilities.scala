package is.clipperz.backend

import is.clipperz.backend.services.{ tollByteSize, PRNG }
import zio.test.Gen
import zio.{ZIO, Task }
import scala.collection.immutable.HashMap
import is.clipperz.backend.services.Session
import zio.nio.file.{ Files, Path }
import zio.stream.ZSink
import is.clipperz.backend.functions.KeyValueStorage.SqlLiteKeyValueStorage
import com.augustnagro.magnum.magzio.*
import zio.s3.S3
import scala.languageFeature.existentials

object TestUtilities:
    def deleteFilesInFolder (path: Path): ZIO[Any, Nothing, Boolean] =
        Files.newDirectoryStream(path).mapZIO { p =>
            for {
                deletedInSubDirectory <- deleteFilesInFolder(p) .whenZIO(Files.isDirectory(p))  .map(_.getOrElse(false))
                deletedFile           <- Files.deleteIfExists(p).whenZIO(Files.isRegularFile(p)).map(_.getOrElse(false))
            } yield deletedInSubDirectory && deletedFile
        }
        .run(ZSink.collectAll)
        .map(_.toArray.foldLeft(true)((a, b) => a && b))
        .catchAll(_ => ZIO.succeed(false))


    def getBytesGen(prng: PRNG, size: Int): Gen[Any, Array[Byte]] =
        Gen.fromZIO(
        prng
            .nextBytes(size)
            .catchAll(_ => ZIO.succeed(Array.emptyByteArray))
        )

    def dropTable(): ZIO[Transactor, Throwable, Unit] = 
        ZIO.service[Transactor].map(
            _.transact:
                sql"drop table if exists UserDb;".update.run()
                sql"drop table if exists OneTimeShareDb;".update.run()
                sql"drop table if exists BlobDb;".update.run()
        )

    def dropBucket(bucketName: String): ZIO[S3, Throwable, Unit] =
        ZIO.service[S3].map(s3 =>
            for{
                objects <- s3.listAllObjects(bucketName).runCollect
                _ <- ZIO.foreachDiscard(objects)(obj => s3.deleteObject(bucketName, obj.key))
                _ <- s3.deleteBucket(bucketName)
                _ <- s3.createBucket(bucketName)
            }yield()
        )