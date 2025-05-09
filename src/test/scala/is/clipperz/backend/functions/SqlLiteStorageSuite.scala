package is.clipperz.backend.functions

import zio.test.ZIOSpecDefault
import zio.stream.ZStream
import zio.test.TestAspect

object SqlLiteStorageSpec extends ZIOSpecDefault:
  val tableName = "blobs"
  val keyValueStorage = KeyValueStorage.SqlLiteKeyValueStorage(tableName)
  
  val testContent = ZStream.fromIterable("testContent".getBytes().nn)
  val failingContent = ZStream.never
  val testKey = "testKey"
  val failingKey = "failingKey"

  def spec = suite("SqlLiteValueStorage")(
    test("getBlob - fail") {
      
    } +
    test("saveBlob - success") {
        for {
          fiber <- keyValueStorage.flatMap(_.saveBlob(testKey, testContent).fork)
          _ <- TestClock.adjust(Duration.fromMillis(KeyValueStorage.WAIT_TIME + 10))
          _ <- fiber.join
          (content, _) <- keyValueStorage.flatMap(_.getBlob(testKey))
          result <- testContent.zip(content).map((a, b) => a == b).toIterator.map(_.map(_.getOrElse(false)).reduce(_ && _))
        } yield assertTrue(result)
    } +
    test("saveBlob with failing stream - success") {

    } +
    test("getBlob - success") {

    } +
    test("deleteBlob - success") {

    } +
    test("deleteBlob - nothing to delete") {

    }
  ) @@
    TestAspect.sequential @@
    TestAspect.beforeAll(TestUtilities.deleteFilesInFolder(blobBasePath)) @@
    TestAspect.afterAll (TestUtilities.deleteFilesInFolder(blobBasePath))
