package is.clipperz.backend.functions

import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO, Scope, Exit }
import zio.stream.ZStream
import zio.test.*
import zio.test.Assertion.*
import zio.json.EncoderOps
import zio.http.*
import zio.nio.file.Path
import zio.nio.file.FileSystem
import is.clipperz.backend.Main
import _root_.is.clipperz.backend.Exceptions.*
import zio.Clock
import zio.test.TestClock
import zio.Duration
import is.clipperz.backend.TestUtilities
import is.clipperz.backend.functions.KeyValueStorageSpec.keyValueStorage
import is.clipperz.backend.otel.PropagatorProvider
import zio.telemetry.opentelemetry.tracing.Tracing
import zio.ZLayer
import zio.telemetry.opentelemetry.OpenTelemetry
import io.opentelemetry.context.ContextStorage
import is.clipperz.backend.otel.OtelSdk
import zio.nio.file.{Path as ZPath}
import zio.s3.S3
import zio.nio.file.Files

object MinIOStorageSpec extends ZIOSpecDefault:

  private val root = FileSystem.default.getPath("target", "s3Test")

  private val s3: ZLayer[Any, Nothing, S3] = zio.s3.stub(root)

  val testContent  = ZStream.fromIterable("testContent".getBytes.nn)
  val testMetadata = ZStream.fromIterable("testMetadata".getBytes.nn)
  val failingContent = ZStream.never
  val testKey = "testKey"
  val failingKey = "failingKey"
  val levels = 16
  val sourceName = "test"
  val instrumentationScopeName = "testScope"
  val tracing = ((OtelSdk.custom("Test") ++ OpenTelemetry.contextZIO) >>> OpenTelemetry.tracing("LoginSpec"))

    val environment =
        tracing ++
        PropagatorProvider.live() ++
        Scope.default

  def storageSuite(name: String, bucketName: String) =

    val keyValueStorage =
          KeyValueStorage.MinIOKeyValueStorage(bucketName).provideLayer(s3)

    suite(s"SqlLiteValueStorage - $name")(
      test("getBlob - fail") {
        assertZIO(keyValueStorage.flatMap(_.getBlob(testKey).exit))(fails(isSubtype[ResourceNotFoundException](anything)))
      },
      test("saveBlob - success") {
        for {
          _ <- keyValueStorage.flatMap(_.saveBlobWithMetadata(testKey, testContent, testMetadata, false))
          _ <- TestClock.adjust(Duration.fromMillis(KeyValueStorage.WAIT_TIME + 10))
          (content, _) <- keyValueStorage.flatMap(_.getBlob(testKey))
          result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
        } yield assertTrue(result)
      },
      test("saveBlob with failing stream - success") {
        for {
          fiber <- keyValueStorage.flatMap(_.saveBlob(failingKey, failingContent, false).fork)
          _     <- TestClock.adjust(Duration.fromMillis(KeyValueStorage.WAIT_TIME + 10))
          res   <- assertZIO(fiber.await)(fails(isSubtype[EmptyContentException](anything)))
        } yield res
      },
      test("getBlob - success") {
        for {
          (content, _) <- keyValueStorage.flatMap(_.getBlob(testKey))
          result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
        } yield assertTrue(result)
      },
      test("deleteBlob - success") {
        for {
          _   <- keyValueStorage.flatMap(_.getBlob(testKey))
          _   <- keyValueStorage.flatMap(_.deleteBlob(testKey))
          res <- assertZIO(keyValueStorage.flatMap(_.getBlob(testKey).exit))(fails(isSubtype[ResourceNotFoundException](anything)))
        } yield TestResult.allSuccesses(assertCompletes, res)
      },
      test("deleteBlob - nothing to delete") {
        for {
          res <- assertZIO(keyValueStorage.flatMap(_.getBlob(testKey).exit))(fails(isSubtype[ResourceNotFoundException](anything)))
          _   <- keyValueStorage.flatMap(_.deleteBlob(testKey))
        } yield TestResult.allSuccesses(res, assertCompletes)
      }
    ) @@ TestAspect.sequential 
    @@ TestAspect.beforeAll({
      for { 
        _ <- TestUtilities.dropBucket(bucketName) 
              .provideLayer(s3) 
      } yield() 
    })

  def spec = suite("All Storages")(
    storageSuite("UserDb", "users"),
    storageSuite("BlobDb", "blobs"),
    storageSuite("OneTimeShareDb", "one-time-shares")
  ).provideLayerShared(environment)
