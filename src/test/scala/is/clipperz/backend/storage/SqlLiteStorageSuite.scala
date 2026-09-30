package is.clipperz.backend.storage

import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO, Scope, Exit }
import zio.stream.ZStream
import zio.test.*
import zio.test.Assertion.*
import zio.json.EncoderOps
import zio.http.*
import zio.nio.file.FileSystem
import zio.nio.file.Files
import is.clipperz.backend.Main
import _root_.is.clipperz.backend.Exceptions.*
import zio.Clock
import zio.test.TestClock
import zio.Duration
import is.clipperz.backend.TestUtilities
import is.clipperz.backend.sqlite.*
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import com.augustnagro.magnum.magzio.Transactor
import com.augustnagro.magnum.magzio.*
import is.clipperz.backend.otel.OtelSdk
import zio.telemetry.opentelemetry.OpenTelemetry
import is.clipperz.backend.storage.SqlLiteKeyValueStorage
import is.clipperz.backend.storage.WAIT_TIME

object SqlLiteStorageSpec extends ZIOSpecDefault:

  val config = new HikariConfig()
  config.setJdbcUrl("jdbc:sqlite:target/testDb.db")
  config.setDriverClassName("org.sqlite.JDBC")
  config.setMaximumPoolSize(5)
  config.setConnectionTestQuery("SELECT 1")

  val dataSource = new HikariDataSource(config)
  val transactor = Transactor.layer(dataSource)

  val testContent  = ZStream.fromIterable("testContent".getBytes.nn)
  val testMetadata = ZStream.fromIterable("testMetadata".getBytes.nn)
  val failingContent = ZStream.never
  val testKey = "testKey"
  val failingKey = "failingKey"

  val tracing = ((OtelSdk.custom("Test", "Test") ++ OpenTelemetry.contextZIO) >>> OpenTelemetry.tracing("LoginSpec"))
  val environment = tracing ++ Scope.default

  private val metricsConfig = new HikariConfig()
  metricsConfig.setJdbcUrl("jdbc:sqlite:target/metricsTestDb.db")
  metricsConfig.setDriverClassName("org.sqlite.JDBC")
  metricsConfig.setMaximumPoolSize(5)
  metricsConfig.setConnectionTestQuery("SELECT 1")

  private val metricsDataSource = new HikariDataSource(metricsConfig)
  private val metricsTransactor = Transactor.layer(metricsDataSource)

  private val metricsRepo = new UserRepo()

  private val metricsTestFilePath =
    FileSystem.default.getPath("./src/test/resources/sizeTest/singleFile/4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63.blob")

  private def insertMetricsFile(transactorService: Transactor, hash: String) =
    for
        content <- Files.readAllBytes(metricsTestFilePath)
        _       <- metricsRepo.createTable(transactorService)
        _       <- transactorService.transact:
                        metricsRepo.deleteById(hash)
        _       <- transactorService.transact:
                        metricsRepo.insert(UserDb(hash, "", content.toArray))
    yield ()

  def metricsSuite =
    suite("SQLiteMetrics")(
      test("countFileNumber - table with rows") {
        for
            transactorService <- ZIO.service[Transactor]
            _                 <- insertMetricsFile(transactorService, "hash1")
            storage           <- SqlLiteKeyValueStorage[UserDb](metricsRepo, UserDb.apply)
            result            <- storage.collect
        yield assertTrue(result.get("files.count").get == 1, result.get("files.size").get == 1018269)
      }
    ).provideLayer(metricsTransactor) @@ TestAspect.sequential

  def storageSuite[T <: DbTable](name: String, repo: Repo[T, T, Key], ctor: (Key, String, Array[Byte]) => T) =
    suite(s"SqlLiteValueStorage - $name")(
      test("getBlob - fail") {
        val keyValueStorage = SqlLiteKeyValueStorage[T](repo, ctor).provideLayer(transactor)
          
        assertZIO(keyValueStorage.flatMap(_.getBlob(testKey).exit))(fails(isSubtype[ResourceNotFoundException](anything)))
      },
      test("saveBlob - success") {
        val keyValueStorage = SqlLiteKeyValueStorage[T](repo, ctor).provideLayer(transactor)

        for {
          _ <- keyValueStorage.flatMap(_.saveBlobWithMetadata(testKey, testContent, testMetadata, false))
          _ <- TestClock.adjust(Duration.fromMillis(WAIT_TIME + 10))
          (content, _) <- keyValueStorage.flatMap(_.getBlob(testKey))
          result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
        } yield assertTrue(result)
      },
      test("saveBlob with failing stream - success") {
        val keyValueStorage = SqlLiteKeyValueStorage[T](repo, ctor).provideLayer(transactor)

        for {
          fiber <- keyValueStorage.flatMap(_.saveBlob(failingKey, failingContent, false).fork)
          _     <- TestClock.adjust(Duration.fromMillis(WAIT_TIME + 10))
          res   <- assertZIO(fiber.await)(fails(isSubtype[EmptyContentException](anything)))
        } yield res
      },
      test("getBlob - success") {
        val keyValueStorage = SqlLiteKeyValueStorage[T](repo, ctor).provideLayer(transactor)

        for {
          (content, _) <- keyValueStorage.flatMap(_.getBlob(testKey))
          result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
        } yield assertTrue(result)
      },
      test("deleteBlob - success") {
        val keyValueStorage = SqlLiteKeyValueStorage[T](repo, ctor).provideLayer(transactor)

        for {
          _   <- keyValueStorage.flatMap(_.getBlob(testKey))
          _   <- keyValueStorage.flatMap(_.deleteBlob(testKey))
          res <- assertZIO(keyValueStorage.flatMap(_.getBlob(testKey).exit))(fails(isSubtype[ResourceNotFoundException](anything)))
        } yield TestResult.allSuccesses(assertCompletes, res)
      },
      test("deleteBlob - nothing to delete") {
        val keyValueStorage = SqlLiteKeyValueStorage[T](repo, ctor).provideLayer(transactor)

        for {
          res <- assertZIO(keyValueStorage.flatMap(_.getBlob(testKey).exit))(fails(isSubtype[ResourceNotFoundException](anything)))
          _   <- keyValueStorage.flatMap(_.deleteBlob(testKey))
        } yield TestResult.allSuccesses(res, assertCompletes)
      }
    ).provideLayerShared(environment) @@ TestAspect.sequential 
    @@ TestAspect.beforeAll({
      for { 
        _ <- TestUtilities.dropTable() 
              .provideLayer(Transactor.layer(dataSource)) 
      } yield() 
    })

  def spec = suite("All Storages")(
    storageSuite("UserDb", new UserRepo(), UserDb.apply),
    storageSuite("BlobDb", new BlobRepo(), BlobDb.apply),
    storageSuite("OneTimeShareDb", new OneTimeShareRepo(), OneTimeShareDb.apply),
    metricsSuite
  )
