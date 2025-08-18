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
    def getUser(username: HexString): Task[Option[RemoteUserCard]]
    def saveUser(user: RemoteUserCard, overwrite: Boolean): Task[HexString]
    def deleteUser(c: HexString): Task[Unit]

object UserManager:
    case class KeyValueUserManager(keyBlobStorage: KeyValueStorage) extends UserManager:
        override def getUser(username: HexString): Task[Option[RemoteUserCard]] =
            keyBlobStorage
            .getBlob(username.toString).map(_._1)
            .flatMap(fromStream[RemoteUserCard](_).map(Some.apply))
            .catchSome:
                case ex: ResourceNotFoundException => ZIO.succeed(None)
                case ex => ZIO.fail(ex)

        override def saveUser(userCard: RemoteUserCard, overwrite: Boolean): Task[HexString] =
            def saveUserCard(userCard: RemoteUserCard): Task[HexString] =
                Charset.Standard.utf8.encodeString(userCard.toJson)
                .flatMap(blobChunks =>
                    keyBlobStorage
                    .saveBlob(
                        userCard.c.toString,
                        ZStream.fromChunks(blobChunks),
                    )
                    .map(_ => userCard.c)
                )

            this.getUser(userCard.c).flatMap(optional => if optional.isDefined
                then (if (overwrite) 
                        then saveUserCard(userCard)
                        else ZIO.fail(new ResourceConflictException("User already present")))
                else saveUserCard(userCard)
            )

        override def deleteUser(c: HexString): Task[Unit] =
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

    def sqlLite: ZLayer[Transactor, Throwable, UserManager] = 
        ZLayer.scoped(
            KeyValueStorage.SqlLiteKeyValueStorage[UserDb](new UserRepo(), UserDb.apply)
                .map(KeyValueUserManager(_))
            // for {
            //     sqlLiteKeyValueStorage <- KeyValueStorage.SqlLiteKeyValueStorage[UserDb]("UserDb", new UserRepo(), UserDb.apply)
            // } yield(KeyValueUserManager(sqlLiteKeyValueStorage))
        )
