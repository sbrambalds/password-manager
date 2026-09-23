package is.clipperz.backend.services

import zio.test.{ ZIOSpecDefault, assertTrue, TestAspect }
import zio.ZIO
import com.zaxxer.hikari.{ HikariConfig, HikariDataSource }
import com.augustnagro.magnum.magzio.Transactor
import is.clipperz.backend.sqlite.*
import is.clipperz.backend.functions.MetricsCollector
import is.clipperz.backend.TestUtilities
import zio.nio.file.FileSystem
import zio.nio.file.Files

object SQLiteMetricsSuite extends ZIOSpecDefault:

    private val config = new HikariConfig()
    config.setJdbcUrl("jdbc:sqlite:target/metricsTestDb.db")
    config.setDriverClassName("org.sqlite.JDBC")
    config.setMaximumPoolSize(5)
    config.setConnectionTestQuery("SELECT 1")

    private val dataSource = new HikariDataSource(config)
    private val transactor = Transactor.layer(dataSource)

    private val repo = new UserRepo()

    private val testFilePath =
    FileSystem.default.getPath("./src/test/resources/sizeTest/singleFile/4073041693a9a66983e6ffb75b521310d30e6db60afc0f97d440cb816bce7c63.blob")

    private def insertFile(transactorService: Transactor, hash: String) =
        for
            content <- Files.readAllBytes(testFilePath)
            _       <- repo.createTable(transactorService)
            _       <- transactorService.transact:
                            repo.insert(UserDb(hash, "", content.toArray))
        yield ()


    def spec = suite("SQLiteMetrics")(
        test("countFileNumber - table with rows") {
        for
            transactorService <- ZIO.service[Transactor]
            _                 <- insertFile(transactorService, "hash1")
            result            <- MetricsCollector.SQLiteMetricsCollector(repo, transactorService).collect
        yield assertTrue(result.get("files.count").get == 1, result.get("files.size").get == 1018269)
        }
    ).provideLayer(transactor) @@ TestAspect.sequential
