package is.clipperz.backend.functions

import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.middleware.scheduledFileSystemMetricsCollection

import java.io.{ FileNotFoundException, FileOutputStream }
import zio.nio.file.{ Files, Path }

import zio.{ Duration, Task, ZIO }
import zio.stream.{ ZSink, ZStream }
import zio.http.codec.HttpCodec.Metadata
import zio.http.Header.ContentType
import zio.nio.file.Files.Attribute
import zio.nio.file.Files.Attributes

// ============================================================================

type Key = String

trait KeyValueStorage:
    def saveBlob             (key: Key, content:  ZStream[Any, Throwable, Byte]): Task[Unit]
    def saveBlobWithMetadata (key: Key, content:  ZStream[Any, Throwable, Byte], metadata: ZStream[Any, Throwable, Byte]): Task[Unit]
    def getBlob              (key: Key): Task[(ZStream[Any, Throwable, Byte], Long)]
    def getMetadata          (key: Key): Task[ZStream[Any, Throwable, Byte]]
    def deleteBlob           (key: Key): Task[Unit]

object KeyValueStorage:
    val WAIT_TIME = 10000

    enum ContentType(val value: String):
        case Blob       extends ContentType("blob")
        case Metadata   extends ContentType("metadata")

    def pathForContentType (filename: Key, path: Path, contentType: ContentType): Path = path / s"${filename}.${contentType.value}"

    class FileSystemKeyValueStorage private (basePath: Path, levels: Int) extends KeyValueStorage:

        private def getContent (key: Key, contentType: ContentType): Task[(ZStream[Any, Throwable, Byte], Long)] =
            getBlobPath(key, false)
            .map(path => pathForContentType(key, path, contentType))
            .flatMap(path =>
                Files.exists(path)
                .flatMap(exists => exists match {
                    case true   => (Files.readAllBytes(path).map(ZStream.fromChunk)).zip(Files.size(path))
                    case false  => ZIO.fail(new ResourceNotFoundException("Referenced blob does not exists"))
                })
            )

        override def getBlob (key: Key): Task[(ZStream[Any, Throwable, Byte], Long)] = getContent(key, ContentType.Blob)

        override def getMetadata(key: Key): Task[ZStream[Any, Throwable, Byte]] = getContent(key, ContentType.Metadata).map(_._1)

        private def saveData (key: Key, contentType: ContentType, content: ZStream[Any, Throwable, Byte]): Task[Unit] = 
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

        private def moveFile (key: Key, content: Path): Task[Unit] =
            getBlobPath(key, true)
            .map(path => pathForContentType(key, path, ContentType.Blob))
            .flatMap(path => Files.move(content, path))

        private  def saveMetadata (key: Key, metadata: ZStream[Any, Throwable, Byte]): Task[Unit]   = saveData(key, ContentType.Metadata, metadata)
        override def saveBlob     (key: Key, content:  ZStream[Any, Throwable, Byte]): Task[Unit]   = saveData(key, ContentType.Blob,     content)

        override def saveBlobWithMetadata (key: Key, content: ZStream[Any, Throwable, Byte], metadata: ZStream[Any, Throwable, Byte]): Task[Unit] =
            saveBlob(key, content) <&> saveMetadata(key, metadata)

        override def deleteBlob (key: Key): Task[Unit] =
            getBlobPath(key, false)
                .flatMap(path =>
                    Files.deleteIfExists(pathForContentType(key, path, ContentType.Blob))
                    <&>
                    Files.deleteIfExists(pathForContentType(key, path, ContentType.Metadata))
                )
            // TODO: delete empty folder?
            .foldZIO(err => ZIO.fail(new NonWritableArchiveException(err.toString())), _ => ZIO.succeed(()))

        private def computeBlobPath (key: Key): Path =
            val piecesLength: Int = key.length / levels
            val pieces: IndexedSeq[String] =
                for (i <- 0 to levels - 1)
                yield key.substring(i * piecesLength, i * piecesLength + piecesLength).nn
            basePath / pieces.mkString("/")

        private def getBlobPath (key: Key, createFolders: Boolean): Task[Path] =
            val path: Path = computeBlobPath(key)
            Files.exists(path)
            .zip(Files.isDirectory(path))
            .flatMap((exists, isDirectory) => (exists, isDirectory, createFolders) match {
                case (true,  true,  _    ) => ZIO.succeed(path)
                case (true,  false, _    ) => ZIO.fail(new ResourceNotFoundException(s"Referenced blob does not exists"))
                case (false, _,     false) => ZIO.fail(new ResourceNotFoundException(s"Referenced blob does not exists"))
                case (false, _,     true ) => Files.createDirectories(path) *> ZIO.succeed(path)
            })

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
                *>  scheduledFileSystemMetricsCollection(basePath).forkDaemon
                *>  ZIO.succeed(new FileSystemKeyValueStorage(basePath, levels))
            })