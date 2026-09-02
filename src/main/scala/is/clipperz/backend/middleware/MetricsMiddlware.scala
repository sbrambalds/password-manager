package is.clipperz.backend.middleware

// import java.nio.file.{ Files, Path }
import zio.nio.file.{ Files, Path }
import java.util.concurrent.TimeUnit.{ SECONDS, NANOSECONDS }

import scala.jdk.CollectionConverters.*

import zio.{ Clock, Duration, Chunk, RuntimeFlags, Schedule, Task, Trace, ZIO, durationInt }
import zio.metrics.{ Metric, MetricLabel, MetricKeyType }
import zio.http.{ Handler, HandlerAspect, Method, Middleware, RoutePattern, Response, Request, Routes }
import zio.ZLayer
import software.amazon.awssdk.services.s3.model.S3Exception
import zio.s3.S3
import zio.stream.ZSink
import com.augustnagro.magnum.magzio.Transactor
import is.clipperz.backend.sqlite.DbTable
import is.clipperz.backend.sqlite.Key
import is.clipperz.backend.storage.ArchiveName
import com.augustnagro.magnum.magzio.*
import zio.stream.ZStream
import is.clipperz.backend.sqlite.UserRepo
import is.clipperz.backend.sqlite.BlobRepo
import is.clipperz.backend.sqlite.OneTimeShareRepo

private val nanoToSeconds = 1e-9
private val refreshRate = 30.minutes

// STATIC METRICS

def elapsedTime[E, R](label: String, tags: Set[MetricLabel])(block: => ZIO[E, Throwable, R]): ZIO[E, Throwable, R] =
    for {
        t0     <- ZIO.succeed(System.nanoTime())
        result <- block.tap(_ =>    ZIO.succeed((System.nanoTime() - t0).toDouble * nanoToSeconds)
                                    @@ Metric.summary(s"${label}.elapsedTime", 1.day, 100, 0.03d, Chunk(0.50, 0.75, 0.90, 0.95, 0.98 /*, 0.99, 0.999*/)).tagged(tags)
                        )
    } yield result

def collectFileSystemMetrics (path: Path): Task[(Long, Long, Array[Long])] =
    val archive = ArchiveName.fromFsPath(path)
    // elapsedTime("files", Set(MetricLabel("archive", path.getFileName().nn.toString())))(
    elapsedTime("files", Set(MetricLabel("archive", archive)))(
        // ZIO.attempt(Files.walk(path).nn.iterator().nn.asScala
        Files.walk(path)
            // .map(_.toFile().nn)
            // .filter(file => file.isFile() && !file.isHidden())
            .filterZIO(path => Files.isRegularFile(path).zip(Files.isHidden(path)).map((regular, hidden) => (regular && !hidden)))
            // .map(file => (1, file.length()))
            .mapZIO(path => Files.size(path).map(size => (1, size)))
            //  count, totalSize, [size]
            .runFold((0L, 0L, Array.empty[Long]))((acc, tuple) => ((acc._1 + tuple._1), (acc._2 + tuple._2), acc._3 :+ (tuple._2)))
            @@ Metric.counter("files.count")
                .contramap[(Long, Long, Array[Long])](_._1)
                .tagged("archive", archive)
                .tagged("type", "file_system")
            @@ Metric.counter("files.size")
                .contramap[(Long, Long, Array[Long])](_._2/1000)
                .tagged("archive", archive)
                .tagged("type", "file_system")
    )

def collectS3Metrics (s3: S3, bucketName: String): Task[(Long, Long, Array[Long])] =
    ZIO.scoped(
        for {
            count           <- s3.listAllObjects(bucketName).run(ZSink.count)
            space           <- s3.listAllObjects(bucketName).map(_.size).run(ZSink.sum[Long])
            files           <- s3.listAllObjects(bucketName).map(_.size).run(ZSink.collectAllToSet)
        } yield ((count, space, files.toArray))
    ) @@ Metric.counter("files.count")
        .contramap[(Long, Long, Array[Long])](_._1)
        .tagged("archive", bucketName)
        .tagged("type", "s3")
    @@ Metric.counter("files.size")
        .contramap[(Long, Long, Array[Long])](_._2/1000)
        .tagged("archive", bucketName)
        .tagged("type", "s3")

def collectSqliteMetrics[T <: DbTable](repo: Repo[T, T, Key], transactor: Transactor, tableName: String): Task[(Long, Long, Array[Long])] =
    transactor.transact {
        (
            repo.findAll.length.toLong, 
            repo.findAll.map(_.blob.length.toLong).sum,
            repo.findAll.map(_.blob.length.toLong).toArray
        )
    } @@ Metric.counter("files.count")
        .contramap[(Long, Long, Array[Long])](_._1)
        .tagged("archive", tableName)
        .tagged("type", "sqlite")
    @@ Metric.counter("files.size")
        .contramap[(Long, Long, Array[Long])](_._2/1000)
        .tagged("archive", tableName)
        .tagged("type", "sqlite")

def scheduledFileSystemMetricsCollection (path: Path) =
    collectFileSystemMetrics(path) `repeat` Schedule.fixed(refreshRate)

def scheduledS3MetricsCollection (s3: S3, bucketname: String) =
    collectS3Metrics(s3, bucketname) `repeat` Schedule.fixed(refreshRate)

def scheduledSQLiteMetricsCollection[T <: DbTable](repo: Repo[T, T, Key], transactor: Transactor) =
    refreshSqliteMetrics(repo, transactor) `repeat` Schedule.fixed(refreshRate)

def refreshSqliteMetrics[T <: DbTable](repo: Repo[T, T, Key], transactor: Transactor): Task[(Long, Long, Array[Long])] =
    val tableName = repo match
        case _: UserRepo         => ArchiveName.users
        case _: BlobRepo         => ArchiveName.blobs
        case _: OneTimeShareRepo => ArchiveName.oneTimeShares
    collectSqliteMetrics(repo, transactor, tableName)