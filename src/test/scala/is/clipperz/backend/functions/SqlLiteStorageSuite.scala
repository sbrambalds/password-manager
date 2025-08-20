package is.clipperz.backend.functions

import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO, Scope, Exit }
import zio.stream.ZStream
import zio.test.*
import zio.test.Assertion.*
import zio.json.EncoderOps
import zio.http.*
import zio.nio.file.FileSystem
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

object SqlLiteStorageSpec extends ZIOSpecDefault:

  val config = new HikariConfig()
  config.setJdbcUrl("jdbc:sqlite:target/ClipperzDb.db")
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

  def storageSuite[T <: DbTable](name: String, repo: Repo[T, T, Key], ctor: (Key, String, Array[Byte]) => T) =
    suite(s"SqlLiteValueStorage - $name")(
      test("getBlob - fail") {
        val keyValueStorage =
          KeyValueStorage.SqlLiteKeyValueStorage[T](repo, ctor).provideLayer(transactor)

        assertZIO(keyValueStorage.flatMap(_.getBlob(testKey).exit))(fails(isSubtype[ResourceNotFoundException](anything)))
      },
      test("saveBlob - success") {
        val keyValueStorage =
          KeyValueStorage.SqlLiteKeyValueStorage[T](repo, ctor).provideLayer(transactor)

        for {
          _ <- keyValueStorage.flatMap(_.saveBlobWithMetadata(testKey, testContent, testMetadata, false))
          _ <- TestClock.adjust(Duration.fromMillis(KeyValueStorage.WAIT_TIME + 10))
          (content, _) <- keyValueStorage.flatMap(_.getBlob(testKey))
          result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
        } yield assertTrue(result)
      },
      test("saveBlob with failing stream - success") {
        val keyValueStorage =
          KeyValueStorage.SqlLiteKeyValueStorage[T](repo, ctor).provideLayer(transactor)

        for {
          fiber <- keyValueStorage.flatMap(_.saveBlob(failingKey, failingContent, false).fork)
          _     <- TestClock.adjust(Duration.fromMillis(KeyValueStorage.WAIT_TIME + 10))
          res   <- assertZIO(fiber.await)(fails(isSubtype[EmptyContentException](anything)))
        } yield res
      },
      test("getBlob - success") {
        val keyValueStorage =
          KeyValueStorage.SqlLiteKeyValueStorage[T](repo, ctor).provideLayer(transactor)

        for {
          (content, _) <- keyValueStorage.flatMap(_.getBlob(testKey))
          result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
        } yield assertTrue(result)
      },
      test("deleteBlob - success") {
        val keyValueStorage =
          KeyValueStorage.SqlLiteKeyValueStorage[T](repo, ctor).provideLayer(transactor)

        for {
          _   <- keyValueStorage.flatMap(_.getBlob(testKey))
          _   <- keyValueStorage.flatMap(_.deleteBlob(testKey))
          res <- assertZIO(keyValueStorage.flatMap(_.getBlob(testKey).exit))(fails(isSubtype[ResourceNotFoundException](anything)))
        } yield TestResult.allSuccesses(assertCompletes, res)
      },
      test("deleteBlob - nothing to delete") {
        val keyValueStorage =
          KeyValueStorage.SqlLiteKeyValueStorage[T](repo, ctor).provideLayer(transactor)

        for {
          res <- assertZIO(keyValueStorage.flatMap(_.getBlob(testKey).exit))(fails(isSubtype[ResourceNotFoundException](anything)))
          _   <- keyValueStorage.flatMap(_.deleteBlob(testKey))
        } yield TestResult.allSuccesses(res, assertCompletes)
      }
    ) @@ TestAspect.sequential 
    @@ TestAspect.beforeAll({
      for { 
        _ <- TestUtilities.dropTable() 
              .provideLayer(Transactor.layer(dataSource)) 
      } yield() 
    })

  def spec = suite("All Storages")(
    storageSuite("UserDb", new UserRepo(), UserDb.apply),
    storageSuite("BlobDb", new BlobRepo(), BlobDb.apply),
    storageSuite("OneTimeShareDb", new OneTimeShareRepo(), OneTimeShareDb.apply)
  )
