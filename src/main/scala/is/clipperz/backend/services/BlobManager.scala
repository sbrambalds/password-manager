package is.clipperz.backend.services

import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.functions.crypto.HashFunction
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.Exceptions.*

import java.io.{ FileNotFoundException, IOException, FileOutputStream }
import zio.nio.file.{ Files, Path }
import java.security.MessageDigest

import zio.{ Chunk, Duration, ZIO, ZLayer, Task }
import zio.stream.{ ZStream, ZSink }
import zio.json.{ JsonDecoder, JsonEncoder, DeriveJsonDecoder, DeriveJsonEncoder }
import zio.nio.charset.Charset
import is.clipperz.backend.functions.KeyValueStorage

// ----------------------------------------------------------------------------

type BlobHash = HexString

// ----------------------------------------------------------------------------

// trait CollectMetrics:
//     def collectMetrics (): Void


trait BlobManager:
    def getBlob    (hash: BlobHash): Task[(ZStream[Any, Throwable, Byte], Long)]
    def saveBlob   (hash: BlobHash, identifier: HexString, content: ZStream[Any, Throwable, Byte]): Task[BlobHash]
    def deleteBlob (hash: BlobHash, identifier: HexString): Task[Unit]

object BlobManager:
    val WAIT_TIME = 10000

    case class KeyValueBlobManager(keyValueStorage: KeyValueStorage, tmpDir: Path) extends BlobManager:
        override def getBlob(hash: BlobHash): Task[(ZStream[Any, Throwable, Byte], Long)] =
            keyValueStorage.getBlob(hash.toString)

        override def saveBlob(hash: BlobHash, identifier: HexString, content: ZStream[Any, Throwable, Byte]): Task[BlobHash] =
            ZIO.scoped:
                Files.createTempFileInScoped(dir=tmpDir, suffix=".tmp", prefix=None, fileAttributes = Nil)
                .flatMap { tmpFile => content
                    .timeoutFail(new EmptyContentException)(Duration.fromMillis(WAIT_TIME))
                    .tapSink(ZSink.fromOutputStream(new FileOutputStream(tmpFile.toFile)))
                    .run(ZSink.digest(MessageDigest.getInstance("SHA-256").nn))
                    .map((chunk: Chunk[Byte]) => HexString.bytesToHex(chunk.toArray))
                    .flatMap { hash_ =>
                        if (hash_ == hash)
                            Charset.Standard.utf8.encodeString(identifier.toString())
                            .map(ZStream.fromChunk)
                            .flatMap(identifierStream => 
                                Files.readAllBytes(tmpFile)
                                .map(ZStream.fromChunk)
                                //  TODO: here the file is copied from `tmp` to the final destination; we may opt to just **move** it - Giulio Cesare 29/02/2024
                                .flatMap(contentStream => keyValueStorage
                                    .saveBlobWithMetadata(hash.toString, contentStream, identifierStream)
                                    .map(_ => hash)
                                )
                            )
                        else ZIO.fail(new BadRequestException(s"Hash of content does not match with hash field provided"))
                    }
                    .catchSome:
                        case ex: FileNotFoundException =>
                            val str: String =
                                if ex.getMessage() == null
                                then "The temporary file or the blob could not be saved"
                                else ex.getMessage().nn
                            ZIO.fail(new NonWritableArchiveException(str))
                        case ex: BadRequestException    => ZIO.fail(ex)
                        case ex: EmptyContentException  => ZIO.fail(ex)
                        case ex: Exception              => ZIO.fail(new NonWritableArchiveException(s"${ex}"))
                }

        override def deleteBlob(hash: BlobHash, identifier: HexString): Task[Unit] =
            ZIO.scoped:
                this.getBlobIdentifier(hash)
                    .flatMap(storedIdentifier =>
                        if storedIdentifier == identifier
                        then keyValueStorage.deleteBlob(hash.toString)
                        else ZIO.fail(new BadRequestException(s"Wrong blob identifier provided"))
                    )
            
        private def getBlobIdentifier(hash: BlobHash): Task[HexString] =
            keyValueStorage
                .getMetadata(hash.toString)
                .flatMap(_.run(ZSink.collectAll[Byte]))
                .flatMap(Charset.Standard.utf8.decodeChunk(_))  //  TODO: how are we messing with this data? Why aren't we going directly from byte[] to HexString 🤔
                .map(chunk => chunk.toArray.mkString)
                .map(HexString(_))

  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

    def fileSystem (
        basePath: Path,
        levels: Int,
        requireExistingPath: Boolean = true,
    ): ZLayer[Any, Throwable, BlobManager] =
        val baseTmpPath: Path = basePath / "tmp"
        ZLayer.scoped(
            KeyValueStorage.FileSystemKeyValueStorage(basePath, levels, requireExistingPath)
            .map(KeyValueBlobManager(_, baseTmpPath))
        )

    def sqlLite (tableName: String): ZLayer[Any, Throwable, BlobManager] = ???
        // ZLayer.scoped(
        //     KeyBlobArchive.SqlLiteKeyBlobArchive(tableName)
        //     .map(KeyValueBlobArchive(_)
        // )