package is.clipperz.backend.functions

import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.{ nothing, throws, throwsA, fails, isSubtype, anything }
import zio.test.{ ZIOSpecDefault, assertTrue, assert, assertCompletes, assertZIO, TestAspect }
import zio.json.EncoderOps
import zio.http.{ Version, Headers, Method, URL, Request, Body }
import zio.http.*
import zio.nio.file.{ FileSystem }
import is.clipperz.backend.Main
import _root_.is.clipperz.backend.Exceptions.*
import zio.Clock
import zio.Clock.ClockLive
import zio.test.TestClock
import zio.Duration
import is.clipperz.backend.TestUtilities
import is.clipperz.backend.sqlite.* 
import zio.test.TestResult.{ allSuccesses }
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import com.augustnagro.magnum.magzio.Transactor
import zio.Scope

object SqlLiteStorageSpec extends ZIOSpecDefault:

  val config = new HikariConfig()
  config.setJdbcUrl("jdbc:sqlite:./target/sqlite/ClipperzDb.db") 
  config.setDriverClassName("org.sqlite.JDBC")
  config.setMaximumPoolSize(5)
  config.setConnectionTestQuery("SELECT 1")
  
  val dataSource = new HikariDataSource(config)

  val repo = new BlobRepo()
  val makeBlob: (Key, String, Array[Byte]) => BlobDb = BlobDb.apply

  val keyValueStorage = KeyValueStorage.SqlLiteKeyValueStorage[BlobDb](repo, makeBlob)
    .provideLayer(Transactor.layer(dataSource))
  
  
  val testContent = ZStream.fromIterable("testContent".getBytes().nn)
  val testMetadata =  ZStream.fromIterable("testMetadata".getBytes().nn)
  val failingContent = ZStream.never
  val testKey = "testKey"
  val failingKey = "failingKey"

  def spec = suite("SqlLiteValueStorage")(
    test("getBlob - fail") {
      assertZIO(keyValueStorage.flatMap(_.getBlob(testKey).exit))(fails(isSubtype[ResourceNotFoundException](anything)))
    } +
    test("saveBlob - success") {
        for {
          _ <- keyValueStorage.flatMap(_.saveBlobWithMetadata(testKey, testMetadata, testContent))
          _ <- TestClock.adjust(Duration.fromMillis(KeyValueStorage.WAIT_TIME + 10))
          (content, _) <- keyValueStorage.flatMap(_.getBlob(testKey))
          result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
        } yield assertTrue(result)
    } +
    // test("saveBlob with failing stream - success") {
    //   for {
    //     error <- keyValueStorage.flatMap(_.saveBlob(failingKey, failingContent))
    //     _ <- TestClock.adjust(Duration.fromMillis(KeyValueStorage.WAIT_TIME + 10))
    //     res <- assertZIO(error)(fails(isSubtype[EmptyContentException](anything)))
    //   } yield res
    // } +
    test("getBlob - success") {
        for {
          (content, _) <- keyValueStorage.flatMap(_.getBlob(testKey))
          result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
        } yield assertTrue(result)
    } +
    test("deleteBlob - success") {
        for {
            _   <- keyValueStorage.flatMap(_.getBlob(testKey))
            _   <- keyValueStorage.flatMap(_.deleteBlob(testKey))
            res <- assertZIO(keyValueStorage.flatMap(_.getBlob(testKey).exit))(fails(isSubtype[ResourceNotFoundException](anything)))
        } yield allSuccesses(assertCompletes, res) 
    } +
    test("deleteBlob - nothing to delete") {
        for {
            res <- assertZIO(keyValueStorage.flatMap(_.getBlob(testKey).exit))(fails(isSubtype[ResourceNotFoundException](anything)))
            _   <- keyValueStorage.flatMap(_.deleteBlob(testKey))
        } yield allSuccesses(res, assertCompletes)
    }
  ) @@ TestAspect.sequential @@
    TestAspect.beforeAll({
      for {
        _ <- keyValueStorage.flatMap(_.dropTable())
        _ <- keyValueStorage.flatMap(_.createTable())
      } yield()
    }) 