package is.clipperz.backend.storage

import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.sqlite.* 

import java.io.{ FileNotFoundException, FileOutputStream }
import zio.nio.file.{ Files, Path }

import zio.{ Duration, Task, ZIO, durationInt }
import zio.stream.{ ZSink, ZStream }
import zio.http.codec.HttpCodec.Metadata
import zio.http.Header.ContentType
import zio.nio.file.Files.Attribute
import zio.nio.file.Files.Attributes
import zio.telemetry.opentelemetry.common.Attributes as TelemetryAttributes
import zio.telemetry.opentelemetry.common.Attribute as TelemetryAttribute
import zio.Console.*

import com.augustnagro.magnum.magzio.*
import org.sqlite.SQLiteDataSource
import javax.sql.DataSource
import zio.Scope
import java.sql.Blob
import java.nio.charset.StandardCharsets
import javax.sql.rowset.serial.SerialBlob
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import zio.Unsafe
import java.io.IOException
import zio.Chunk
import scala.collection.mutable.ArraySeq.ofBoolean
import is.clipperz.backend.otel.TracingAspect.MethodTracer
import com.zaxxer.hikari.metrics.MetricsTracker
import is.clipperz.backend.Main.validateEnv
import zio.telemetry.opentelemetry.tracing.Tracing
import is.clipperz.backend.otel.PropagatorProvider
import zio.s3.S3Bucket
import zio.s3.Live
import software.amazon.awssdk.services.s3.S3AsyncClient
import zio.s3.S3
import software.amazon.awssdk.services.s3.model.S3Exception
import zio.stm.ZSTM
import software.amazon.awssdk.utils.Md5Utils
import zio.telemetry.opentelemetry.metrics.Meter
import zio.s3.S3ObjectSummary
import zio.metrics.Metric
import zio.metrics.MetricLabel
import zio.Schedule
import zio.nio.file.FileSystem
import is.clipperz.backend.services.UserManager
import is.clipperz.backend.services.BlobManager
import is.clipperz.backend.services.OneTimeShareManager
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials
import software.amazon.awssdk.regions.Region
import java.net.URI
import zio.UIO

// ============================================================================

type Key = String

enum ContentType(val value: String):
    case Blob       extends ContentType("blob")
    case Metadata   extends ContentType("metadata")

val WAIT_TIME = 10000

trait KeyValueStorage:
    private val nanoToSeconds = 1e-9
    private val refreshRate = 1.minute

    protected def archive: String
    protected def storageType: String
    protected def measure: Task[Map[String, Long]]

    final def pathForContentType (filename: Key, path: Path, contentType: ContentType): Path = path / s"${filename}.${contentType.value}"

    final def collect: Task[Map[String, Long]] =
        elapsedTime("collect", Set(MetricLabel("archive", archive))):
            measure.tap(metrics =>
                ZIO.foreachDiscard(metrics):
                    case (name, value) =>
                        Metric.gauge(name)
                            .tagged(MetricLabel("archive", archive), MetricLabel("storage", storageType))
                            .set(value.toDouble)
            )

    final def scheduled = collect `repeat` Schedule.fixed(refreshRate)

    final def elapsedTime[E, R](label: String, tags: Set[MetricLabel])(block: => ZIO[E, Throwable, R]): ZIO[E, Throwable, R] =
        for
            t0      <- ZIO.succeed(System.nanoTime())
            result  <- block.tap(_ => ZIO.succeed((System.nanoTime() - t0).toDouble * nanoToSeconds)
                                    @@ Metric.summary(s"${label}.elapsedTime", 1.day, 100, 0.03d, Chunk(0.50, 0.75, 0.90, 0.95, 0.98)).tagged(tags))
        yield result

    def saveBlobWithMetadata (key: Key, content:  ZStream[Any, Throwable, Byte], metadata: ZStream[Any, Throwable, Byte], overwrite: Boolean): ZIO[Tracing, Throwable, Unit]
    def getBlob              (key: Key): ZIO[Tracing, Throwable, (ZStream[Any, Throwable, Byte], Long)]
    def getMetadata          (key: Key): ZIO[Tracing, Throwable, ZStream[Any, Throwable, Byte]]
    def deleteBlob           (key: Key): ZIO[Tracing, Throwable, Unit]
    def saveBlob             (key: Key, content: ZStream[Any, Throwable, Byte], overwrite: Boolean): ZIO[Tracing, Throwable, Unit]

object KeyValueStorage:

    val keyValueStorageFolderDepth = 16

    def apply[T >: KeyValueStorage](args: Chunk[String], archive: String): Either[zio.UIO[Unit], ZIO[Scope, Throwable, T]] =
        args(0) match
            case "fileSystem" =>
                if args.length == 5
                then
                    val blobBasePath         = FileSystem.default.getPath(args(2))
                    val userBasePath         = FileSystem.default.getPath(args(3))
                    val oneTimeShareBasePath = FileSystem.default.getPath(args(4))

                    Right(archive match 
                        case "user"     =>  FileSystemKeyValueStorage(userBasePath, keyValueStorageFolderDepth, true)
                        case "blob"     =>  FileSystemKeyValueStorage(blobBasePath, keyValueStorageFolderDepth, true)
                        case "shares"   =>  FileSystemKeyValueStorage(oneTimeShareBasePath, keyValueStorageFolderDepth, true)
                    )
                    
                else Left(ZIO.logFatal("Not enough arguments"))
            case "db" =>
                if args.length == 3
                then
                    val blobPath = FileSystem.default.getPath("target/blobs")

                    val dataSourceConfig = new HikariConfig()
                    dataSourceConfig.setJdbcUrl("jdbc:sqlite:" + args(2) + "clipperzDb.db")
                    dataSourceConfig.setDriverClassName("org.sqlite.JDBC")
                    dataSourceConfig.setMaximumPoolSize(1) 
                
                    val dataSource = new HikariDataSource(dataSourceConfig)
                    val transactor = Transactor.layer(dataSource)

                    Right(archive match 
                            case "user"     =>  SqlLiteKeyValueStorage(new UserRepo(), UserDb.apply).provideLayer(transactor)  
                            case "blob"     =>  SqlLiteKeyValueStorage(new BlobRepo(), BlobDb.apply).provideLayer(transactor)  
                            case "shares"   =>  SqlLiteKeyValueStorage(new OneTimeShareRepo(), OneTimeShareDb.apply).provideLayer(transactor)  
                    )

                else Left(ZIO.logFatal("Not enough arguments"))
            case "s3" =>
                if args.length == 3
                then
                    val blobPath = FileSystem.default.getPath("target/blobs")

                    val s3 = zio.s3
                                .live(
                                    Region.US_EAST_1,
                                    AwsBasicCredentials.create("admin", "Password123"),
                                    Some(URI(sys.env.getOrElse("RUSTFS_ENDPOINT", "http://localhost:9000"))),
                                    forcePathStyle = Some(true)
                                )
                    
                    Right(s3.build.flatMap(env => RustFSKeyValueStorage(archive).provideEnvironment(env)))

                else Left(ZIO.logFatal("Not enough arguments"))

            case _ => Left(ZIO.logFatal("Error during running"))

        