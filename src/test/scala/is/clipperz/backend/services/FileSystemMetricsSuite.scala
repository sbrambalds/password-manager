package is.clipperz.backend.services

import zio.test.ZIOSpecDefault
import zio.Scope
import zio.test.Spec
import zio.test.{ ZIOSpecDefault, assertTrue, assert, assertCompletes, assertZIO, TestAspect }
import zio.nio.file.{ FileSystem }
import zio.ZIO

import is.clipperz.backend.middleware.collectFileSystemMetrics

object FileSystemMetricsSuite extends ZIOSpecDefault:

  def spec = suite("FileSystemMetrics")(
    test("countFileNumber - folder with one file") {
      for {
        result <- collectFileSystemMetrics(FileSystem.default.getPath("./src/test/resources/sizeTest/singleFile"))
      } yield assertTrue(result._1 == 1, result._2 == 1018269)
    } + 
    test("countFileNumber - single nested folders") {
      for {
        result <- collectFileSystemMetrics(FileSystem.default.getPath("./src/test/resources/sizeTest/singleNestedFolders"))
      } yield assertTrue(result._1 == 15, result._2 == 2304)
    } + 
    test("countFileNumber - multiple nested folders") {
      for {
        result <- collectFileSystemMetrics(FileSystem.default.getPath("./src/test/resources/sizeTest/multipleNestedFolders"))
      } yield assertTrue(result._1 == 23, result._2 == 3376)
    }
  )

