package is.clipperz.backend.functions

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
import zio.stream.ZStream
import com.augustnagro.magnum.Repo
import is.clipperz.backend.sqlite.*

private val nanoToSeconds = 1e-9
private val refreshRate = 30.minutes


trait MetricsCollector:
    protected def archive: String
    protected def measure: Task[Map[String, Long]]

    final def collect: Task[Map[String, Long]] =
        measure.tap(metrics =>
            ZIO.foreachDiscard(metrics):
                case (name, value) =>
                    Metric.counter(name)
                        .tagged(MetricLabel("archive", archive))
                        .update(value)
        )

    // final def scheduled = collect `repeat` Schedule.fixed(refreshRate)

    final def elapsedTime[E, R](label: String, tags: Set[MetricLabel])(block: => ZIO[E, Throwable, R]): ZIO[E, Throwable, R] =
        for
            t0     <- ZIO.succeed(System.nanoTime())
            result <- block.tap(_ => ZIO.succeed((System.nanoTime() - t0).toDouble * nanoToSeconds)
                                    @@ Metric.summary(s"${label}.elapsedTime", 1.day, 100, 0.03d, Chunk(0.50, 0.75, 0.90, 0.95, 0.98)).tagged(tags))
        yield result

object MetricsCollector:
    
    case class FileSystemMetricsCollector(basePath: Path) extends MetricsCollector:
        protected def archive: String = basePath.filename.toString()
        protected def measure: Task[Map[String, Long]] =
            for {
                (count, space)  <-  elapsedTime("files", Set(MetricLabel("archive", archive)))(
                                        Files.walk(basePath)
                                        .filterZIO(path => Files.isRegularFile(path).zip(Files.isHidden(path)).map((regular, hidden) => (regular && !hidden)))
                                        .mapZIO(path => Files.size(path).map(size => (1, size)))
                                        .runFold((0L, 0L))((acc, tuple) => ((acc._1 + tuple._1), (acc._2 + tuple._2)))
                                    )
            } yield(Map("files.count" -> count, "files.size" -> space))
            

    case class S3MetricsCollector(s3: S3, bucketName: String) extends MetricsCollector:
        protected def archive: String = bucketName
        protected def measure: Task[Map[String, Long]] = 
            val files = s3.listAllObjects(bucketName)
            ZIO.scoped(
                for {
                    count           <- files.run(ZSink.count)
                    space           <- files.map(_.size).run(ZSink.sum[Long])
                } yield (Map("files.count" -> count, "files.size" -> space))
            )

    case class SQLiteMetricsCollector[T <: DbTable](repo: Repo[T, T, Key], transactor: Transactor) extends MetricsCollector:
        protected def archive: String = repo.name
        protected def measure: Task[Map[String, Long]] = 
            transactor.transact {
                val all = repo.findAll
                Map(
                    "files.count" -> all.length.toLong, 
                    "files.size" -> all.map(_.blob.length.toLong).sum
                )
            }