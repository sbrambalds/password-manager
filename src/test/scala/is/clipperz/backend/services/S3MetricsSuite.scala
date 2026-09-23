package is.clipperz.backend.services

import zio.test.{ ZIOSpecDefault, assertTrue, TestAspect }
import zio.{ ZIO, ZLayer }
import zio.nio.file.FileSystem
import zio.s3.S3
import zio.stream.ZStream
import is.clipperz.backend.functions.MetricsCollector
import is.clipperz.backend.TestUtilities

object S3MetricsSuite extends ZIOSpecDefault:

  private val root = FileSystem.default.getPath("./src/test/resources/sizeTest")
  private val s3: ZLayer[Any, Nothing, S3] = zio.s3.stub(root)
  private val bucketName = "singleFile"

  def spec = suite("S3Metrics")(
    test("countFileNumber - bucket with files") {
      for
          s3Service <- ZIO.service[S3]
          result    <- MetricsCollector.S3MetricsCollector(s3Service, bucketName).collect
      yield assertTrue(result.get("files.count").get == 1, result.get("files.size").get == 1018269)
    }
  ).provideLayer(s3) @@ TestAspect.sequential

