package is.clipperz.backend.services

import java.io.File
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.{ nothing, throws, throwsA, fails, isSubtype, anything }
import zio.test.{ ZIOSpecDefault, assertTrue, assert, assertCompletes, assertZIO, TestAspect }
import zio.json.EncoderOps
import zio.http.{ Version, Headers, Method, URL, Request, Body }
import zio.http.*
import zio.nio.file.{ Files, FileSystem }
import is.clipperz.backend.Main
import _root_.is.clipperz.backend.Exceptions.*
import zio.Clock
import zio.Clock.ClockLive
import zio.test.TestClock
import zio.Duration
import is.clipperz.backend.data.HexString
import zio.test.TestEnvironment
import zio.ZLayer
import is.clipperz.backend.TestUtilities

object BlobManagerSpec extends ZIOSpecDefault:
  val blobBasePath = FileSystem.default.getPath("target", "tests", "archive", "blobs")

  val keyBlobManagerFolderDepth = 16
  val environment = BlobManager.fileSystem(blobBasePath, keyBlobManagerFolderDepth, false)

  val testContent = ZStream.fromIterable("testContent".getBytes().nn)
  val failingContent = ZStream.never
  val testKey = HexString("d1d733a8041744d6e4b7b991b5f38df48a3767acd674c9df231c92068801a460")
  val failingKey = HexString("d1d733a8041744d6e4b7b991b5f38df48a3767acd674c9df231c92068801a789")
  val identifier = HexString("abba")
  def spec = suite("BlobManager")(
    test("getBlob - fail") {
      for {
        archive <- ZIO.service[BlobManager]
        res <- assertZIO(archive.getBlob(testKey).exit)(fails(isSubtype[ResourceNotFoundException](anything)))
      } yield res
    } +
      test("saveBlob - success") {
        for {
          archive <- ZIO.service[BlobManager]
          fiber <- archive.saveBlob(testKey, identifier, testContent).fork
          _ <- TestClock.adjust(Duration.fromMillis(BlobManager.WAIT_TIME + 10))
          _ <- fiber.join
          (content, _) <- archive.getBlob(testKey)
          result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
        } yield assertTrue(result)
      } +
      test("saveBlob with failing stream - success") {
        for {
          archive <- ZIO.service[BlobManager]
          fiber <- archive.saveBlob(failingKey, identifier, failingContent).fork
          _ <- TestClock.adjust(Duration.fromMillis(BlobManager.WAIT_TIME + 10))
          res <- assertZIO(fiber.await)(fails(isSubtype[EmptyContentException](anything)))
        } yield res
      } +
      test("saveBlob with wrong hash - success") {
        for {
          archive <- ZIO.service[BlobManager]
          fiber <- archive.saveBlob(failingKey, identifier, testContent).fork
          _ <- TestClock.adjust(Duration.fromMillis(BlobManager.WAIT_TIME + 10))
          res <- assertZIO(fiber.await)(fails(isSubtype[BadRequestException](anything)))
        } yield res
      } +
      test("getBlob - success") {
        for {
          archive <- ZIO.service[BlobManager]
          (content, _) <- archive.getBlob(testKey)
          result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
        } yield assertTrue(result)
      } +
      test("deleteBlob - success") {
        for {
          archive <- ZIO.service[BlobManager]
          _       <- archive.deleteBlob(testKey, identifier)
        } yield assertCompletes
      } +
      test("deleteBlob - fail") {
        for {
          archive <- ZIO.service[BlobManager]
          res     <- assertZIO(archive.deleteBlob(testKey, identifier).exit)(fails(isSubtype[ResourceNotFoundException](anything)))
        } yield res
      }
  ).provideSomeLayerShared(environment) @@
    TestAspect.sequential @@
    TestAspect.beforeAll(TestUtilities.deleteFilesInFolder(blobBasePath))
    TestAspect.afterAll (TestUtilities.deleteFilesInFolder(blobBasePath))
