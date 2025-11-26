package is.clipperz.backend.services

import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.functions.KeyValueStorage
import is.clipperz.backend.Exceptions.{ BadRequestException, ResourceConflictException, ResourceNotFoundException }

import zio.nio.file.Path
import zio.nio.charset.Charset

import zio.{ ZIO, ZLayer, Tag, Task, Chunk }
import zio.json.{ JsonDecoder, JsonEncoder, DeriveJsonDecoder, DeriveJsonEncoder, EncoderOps }
import zio.stream.{ ZSink, ZStream }
import is.clipperz.backend.sqlite.Key
import is.clipperz.backend.sqlite.DbTable
import com.augustnagro.magnum.Repo
import com.augustnagro.magnum.magzio.Transactor
import is.clipperz.backend.sqlite.UserRepo
import is.clipperz.backend.sqlite.UserDb
import com.augustnagro.magnum.magzio.sql
import zio.telemetry.opentelemetry.tracing.Tracing
import software.amazon.awssdk.services.s3.S3AsyncClient
import zio.s3.S3
import zio.s3.Live
import software.amazon.awssdk.services.s3.model.S3Exception

// ============================================================================

case class MasterKeyEncodingVersion (
    tag: String
)

object MasterKeyEncodingVersion:
    implicit val decoder: JsonDecoder[MasterKeyEncodingVersion] = DeriveJsonDecoder.gen[MasterKeyEncodingVersion]
    implicit val encoder: JsonEncoder[MasterKeyEncodingVersion] = DeriveJsonEncoder.gen[MasterKeyEncodingVersion]
  
case class SRPVersion(
    tag: String
)
      
object SRPVersion:
    implicit val decoder: JsonDecoder[SRPVersion] = DeriveJsonDecoder.gen[SRPVersion]
    implicit val encoder: JsonEncoder[SRPVersion] = DeriveJsonEncoder.gen[SRPVersion]

case class RequestUserCard (
    c: HexString,
    s: HexString,
    v: HexString,
    srpVersion: SRPVersion,
    originMasterKey: Option[HexString],
    masterKey: (HexString, MasterKeyEncodingVersion)
)

object RequestUserCard:
    implicit val decoder: JsonDecoder[RequestUserCard] = DeriveJsonDecoder.gen[RequestUserCard]
    implicit val encoder: JsonEncoder[RequestUserCard] = DeriveJsonEncoder.gen[RequestUserCard]

case class RemoteUserCard (
    c: HexString,
    s: HexString,
    v: HexString,
    srpVersion: SRPVersion,
    masterKey: (HexString, MasterKeyEncodingVersion)
)

object RemoteUserCard:
    implicit val decoder: JsonDecoder[RemoteUserCard] = DeriveJsonDecoder.gen[RemoteUserCard]
    implicit val encoder: JsonEncoder[RemoteUserCard] = DeriveJsonEncoder.gen[RemoteUserCard]

def remoteFromRequest(requestUserCard : RequestUserCard): RemoteUserCard =
    RemoteUserCard(
        requestUserCard.c,
        requestUserCard.s,
        requestUserCard.v,
        requestUserCard.srpVersion,
        requestUserCard.masterKey
    )

case class UserCard(
    originMasterKey: HexString,
    masterKey: (HexString, MasterKeyEncodingVersion)
)

object UserCard:
    implicit val decoder: JsonDecoder[UserCard] = DeriveJsonDecoder.gen[UserCard]

// ============================================================================

trait UserManager:
    def getUser(username: HexString): ZIO[Tracing, Throwable, Option[RemoteUserCard]]
    def saveUser(user: RemoteUserCard, overwrite: Boolean): ZIO[Tracing, Throwable, HexString]
    def deleteUser(c: HexString): ZIO[Tracing, Throwable, Unit]

object UserManager:
    case class KeyValueUserManager(keyBlobStorage: KeyValueStorage) extends UserManager:
        override def getUser(username: HexString): ZIO[Tracing, Throwable, Option[RemoteUserCard]] =
            keyBlobStorage
            .getBlob(username.toString).map(_._1)
            .flatMap(fromStream[RemoteUserCard](_).map(Some.apply))
            .catchSome:
                case ex: ResourceNotFoundException => ZIO.succeed(None)
                case ex => ZIO.fail(ex)

        override def saveUser(userCard: RemoteUserCard, overwrite: Boolean): ZIO[Tracing, Throwable, HexString] =
            Charset.Standard.utf8.encodeString(userCard.toJson)
            .flatMap(blobChunks =>
                keyBlobStorage
                .saveBlob(
                    userCard.c.toString,
                    ZStream.fromChunks(blobChunks),
                    overwrite
                )
                .map(_ => userCard.c)
            )

        override def deleteUser(c: HexString): ZIO[Tracing, Throwable, Unit] =
            this
            .getUser(c)
            .flatMap(optional =>
                if optional.isDefined
                then keyBlobStorage.deleteBlob(c.toString)
                else ZIO.fail(new ResourceNotFoundException("User does not exist"))
            )

    def fileSystem(
        basePath: Path,
        levels: Int,
        requireExistingPath: Boolean = true,
    ): ZLayer[Any, Throwable, UserManager] =
        ZLayer.scoped(
            KeyValueStorage.FileSystemKeyValueStorage(basePath, levels, requireExistingPath).map(new KeyValueUserManager(_))
        )

    def sqlLite(transactor: ZLayer[Any, Nothing, Transactor]): ZLayer[Any, Throwable, UserManager] = 
        ZLayer.scoped(
            KeyValueStorage.SqlLiteKeyValueStorage[UserDb](new UserRepo(), UserDb.apply)
                .map(KeyValueUserManager(_))
                .provideLayer(transactor)
        )

    def minIO(
        s3: ZLayer[Any, S3Exception, S3],
        levels: Int
    ): ZLayer[S3, Throwable, UserManager] =
                ZLayer.scoped(
            KeyValueStorage.MinIOKeyValueStorage("users")
            .map(new KeyValueUserManager(_))
            .provideLayer(s3)
        )