package is.clipperz.backend.services

import com.github.nscala_time.time.Imports.DateTime
import com.github.nscala_time.time.StaticDateTimeFormat

import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.{ bytesToHex }
import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.functions.crypto.HashFunction
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.functions.KeyValueStorage

import java.io.{ FileNotFoundException, IOException }
import zio.nio.file.{ Files, Path }
import java.security.MessageDigest
import java.util.UUID

import zio.{ Duration, ZIO, ZLayer, Task, Chunk }
import zio.json.{ JsonDecoder, JsonEncoder, DeriveJsonDecoder, DeriveJsonEncoder }
import zio.stream.{ ZStream, ZSink }
import is.clipperz.backend.apis.SecretVersion
import com.augustnagro.magnum.magzio.Transactor
import is.clipperz.backend.sqlite.Key
import is.clipperz.backend.sqlite.DbTable
import com.augustnagro.magnum.Repo
// import is.clipperz.backend.sqlite.
import is.clipperz.backend.sqlite.OneTimeShareDb
import is.clipperz.backend.sqlite.OneTimeShareRepo
import zio.telemetry.opentelemetry.tracing.Tracing
import software.amazon.awssdk.services.s3.S3AsyncClient
import zio.s3.S3
import zio.s3.Live
import software.amazon.awssdk.services.s3.model.S3Exception

// ----------------------------------------------------------------------------

type SecretId = String

case class OneTimeSecret(
    secret:         HexString,
    expirationDate: DateTime,
    version:        Option[SecretVersion]
)

object OneTimeSecret:
    implicit val decoder: JsonDecoder[OneTimeSecret] = DeriveJsonDecoder.gen[OneTimeSecret]
    implicit val encoder: JsonEncoder[OneTimeSecret] = DeriveJsonEncoder.gen[OneTimeSecret]

implicit val decoder: JsonDecoder[DateTime] = JsonDecoder[String].map(DateTime.parse(_))
implicit val encoder: JsonEncoder[DateTime] = JsonEncoder[String].contramap(_.toString())

// ----------------------------------------------------------------------------

trait OneTimeShareManager:
    def getSecret(id: SecretId): ZIO[Tracing, Throwable, (OneTimeSecret, Long)]
    def saveSecret(content: ZStream[Any, Throwable, Byte]): ZIO[Tracing, Throwable, SecretId]
    def deleteSecret(id: SecretId): ZIO[Tracing, Throwable, Unit]

object OneTimeShareManager:

    case class KeyValueOneTimeShareManager(keyBlobArchive: KeyValueStorage) extends OneTimeShareManager:
        override def getSecret(id: SecretId): ZIO[Tracing, Throwable, (OneTimeSecret, Long)] =
            keyBlobArchive.getBlob(id).flatMap((content, contentLength) => fromStream[OneTimeSecret](content).zip(ZIO.succeed(contentLength)))
        
        override def deleteSecret(id: SecretId): ZIO[Tracing, Throwable, Unit] = 
            keyBlobArchive.deleteBlob(id)

        override def saveSecret(content: ZStream[Any, Throwable, Byte]): ZIO[Tracing, Throwable, SecretId] =
            val id = UUID.randomUUID().nn.toString();
            ZIO
                .scoped:
                    keyBlobArchive
                        .saveBlob(id, content, false)
                        .map(_ => id)
                .catchSome:
                    case ex: FileNotFoundException =>
                        val str: String =
                        if ex.getMessage() == null then "The temporary file or the secret could not be saved" else ex.getMessage().nn
                        ZIO.fail(new NonWritableArchiveException(str))
                    case ex: BadRequestException => ZIO.fail(ex)
                    case ex: EmptyContentException => ZIO.fail(ex)
                    case ex => ZIO.fail(new NonWritableArchiveException(s"${ex}"))

  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

    def initializeOneTimeShareManager(basePath: Path): Task[Unit] =

        ZIO.attempt:
            Files.exists(basePath)
            .zip(Files.isDirectory(basePath))
            .flatMap(checks => checks match {
                case (true, false)  => Files.delete(basePath).map(_ => basePath.toFile.mkdirs())
                case (true, true)   => ZIO.succeed(true)
                case (false, _)     => ZIO.succeed(basePath.toFile.mkdirs())
            })
            .map(result =>
                if (result == false)
                    throw new IOException("Failed initialization of temporary blob directory")
            )

    def fileSystem(
        basePath: Path,
        levels: Int,
        requireExistingPath: Boolean = true,
    ): ZLayer[Any, Throwable, OneTimeShareManager] =
        ZLayer.scoped(
            KeyValueStorage.FileSystemKeyValueStorage(basePath, levels, requireExistingPath)
                .map(new KeyValueOneTimeShareManager(_))
        )

    def sqlLite(transactor: ZLayer[Any, Nothing, Transactor]): ZLayer[Any, Throwable, OneTimeShareManager] = 
        ZLayer.scoped(
            KeyValueStorage.SqlLiteKeyValueStorage[OneTimeShareDb](new OneTimeShareRepo(), OneTimeShareDb.apply)
                .map(KeyValueOneTimeShareManager(_))
                .provideLayer(transactor)
        )

    def minIO(
        s3: ZLayer[Any, S3Exception, S3]
    ): ZLayer[Any, Throwable, OneTimeShareManager] =
        ZLayer.scoped(
            KeyValueStorage.MinIOKeyValueStorage("one-time-shares")
                .map(new KeyValueOneTimeShareManager(_))
                .provideLayer(s3)
        )