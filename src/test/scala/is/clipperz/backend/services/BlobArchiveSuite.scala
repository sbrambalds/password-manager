package is.clipperz.backend.services

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.{ nothing, throws, throwsA, fails, isSubtype, anything }
import zio.test.{ ZIOSpecDefault, assertTrue, assert, assertCompletes, assertZIO, TestAspect }
import zio.json.EncoderOps
import zhttp.http.{ Version, Headers, Method, URL, Request, HttpData }
import zhttp.http.*
import is.clipperz.backend.Main
import java.nio.file.Path
import _root_.is.clipperz.backend.exceptions.ResourceNotFoundException
import is.clipperz.backend.functions.FileSystem
import is.clipperz.backend.exceptions.EmptyContentException
import zio.Clock
import zio.Clock.ClockLive
import zio.test.TestClock
import zio.Duration
import is.clipperz.backend.data.HexString
import is.clipperz.backend.exceptions.BadRequestException
import zio.test.TestEnvironment
import zio.ZLayer

object BlobArchiveSpec extends ZIOSpecDefault:
  val blobBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "blobs").nn

  val environment = BlobArchive.fs(blobBasePath, 2)

  val testContent = ZStream.fromIterable("testContent".getBytes().nn)
  val failingContent = ZStream.never
  val testKey = HexString("d1d733a8041744d6e4b7b991b5f38df48a3767acd674c9df231c92068801a460")
  val failingKey = HexString("d1d733a8041744d6e4b7b991b5f38df48a3767acd674c9df231c92068801a789")

  def spec = suite("BlobArchive")(
    test("getBlob - fail") {
      for {
        archive <- ZIO.service[BlobArchive]
        res <- assertZIO(archive.getBlob(testKey).exit)(fails(isSubtype[ResourceNotFoundException](anything)))
      } yield res
    } +
    test("saveBlob - success") {
      for {
        archive <- ZIO.service[BlobArchive]
        fiber <- archive.saveBlob(testKey, testContent).fork
        _ <- TestClock.adjust(Duration.fromMillis(BlobArchive.WAIT_TIME + 10))
        _ <- fiber.join
        content <- archive.getBlob(testKey)
        result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
      } yield assertTrue(result)
    } + 
    test ("saveBlob with failing stream - success") {
      for {
       archive <- ZIO.service[BlobArchive]
       fiber <- archive.saveBlob(failingKey, failingContent).fork
        _ <- TestClock.adjust(Duration.fromMillis(BlobArchive.WAIT_TIME + 10))
        res <- assertZIO(fiber.await)(fails(isSubtype[EmptyContentException](anything)))
      } yield res
    } + 
    test ("saveBlob with wrong hash - success") {
      for {
       archive <- ZIO.service[BlobArchive]
       fiber <- archive.saveBlob(failingKey, testContent).fork
        _ <- TestClock.adjust(Duration.fromMillis(BlobArchive.WAIT_TIME + 10))
        res <- assertZIO(fiber.await)(fails(isSubtype[BadRequestException](anything)))
      } yield res
    } +
    test("getBlob - success") {
      for {
        archive <- ZIO.service[BlobArchive]
        content <- archive.getBlob(testKey)
        result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
      } yield assertTrue(result)
    } + 
    test("deleteBlob - success") {
      for {
        archive <- ZIO.service[BlobArchive]
        res <- archive.deleteBlob(testContent)
      } yield assertTrue(res)
    } + 
    test("deleteBlob - fail") {
      for {
        archive <- ZIO.service[BlobArchive]
        res <- archive.deleteBlob(testContent)
      } yield assertTrue(!res)
    }
  ).provideSomeLayerShared(environment) @@ 
    TestAspect.sequential @@ 
    TestAspect.beforeAll(ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn))) @@
    TestAspect.afterAll(ZIO.succeed(FileSystem.deleteAllFiles(blobBasePath.toFile().nn)))
