package is.clipperz.backend.services

import zio.test.ZIOSpecDefault
import zio.Scope
import zio.test.Spec
import zio.test.{ ZIOSpecDefault, assertTrue, assert, assertCompletes, assertZIO, TestAspect }
import zio.nio.file.{ FileSystem }
import zio.ZIO

import is.clipperz.backend.functions.MetricsCollector

object FileSystemMetricsSuite extends ZIOSpecDefault:

  def spec = suite("FileSystemMetrics")(
    test("countFileNumber - folder with one file") {
      val metricsCollector = MetricsCollector.FileSystemMetricsCollector(FileSystem.default.getPath("./src/test/resources/sizeTest/singleFile"))
      for
          result  <-  metricsCollector.collect
      yield assertTrue(result.get("files.count").get == 1, result.get("files.size").get == 1018269)
    } +
    test("countFileNumber - single nested folders") {
      val metricsCollector = MetricsCollector.FileSystemMetricsCollector(FileSystem.default.getPath("./src/test/resources/sizeTest/singleNestedFolders"))
      for {
          result  <-  metricsCollector.collect
      } yield(assertTrue(result.get("files.count").get == 15, result.get("files.size").get == 2304))
    } + 
    test("countFileNumber - multiple nested folders") {
      val metricsCollector = MetricsCollector.FileSystemMetricsCollector(FileSystem.default.getPath("./src/test/resources/sizeTest/multipleNestedFolders"))
      for {
          result  <-  metricsCollector.collect
      } yield(assertTrue(result.get("files.count").get == 23, result.get("files.size").get == 3376))
    }
  )

