package is.clipperz.backend

import is.clipperz.backend.apis.{ blobsApi, loginApi, logoutApi, staticApi, usersApi, oneTimeShareApi }
import is.clipperz.backend.functions.{ customErrorHandler }
import is.clipperz.backend.middleware.{ hashcash, metrics }
import is.clipperz.backend.services.{ BlobManager, PRNG, SessionManager, SrpManager, TollManager, UserManager, OneTimeShareManager }
import is.clipperz.backend.services.ChallengeType

import zio.nio.file.{ Files, FileSystem }

import scala.util.Try

import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource

import zio.{ LogLevel, Runtime, Scope, ZIOAppArgs, ZIO, ZLayer, durationInt }
import zio.logging.LogFormat
import zio.metrics.connectors.{ MetricsConfig, datadog }
import zio.http.{ Middleware, Server }
import zio.http.netty.{ EventLoopGroups, NettyConfig }
import zio.http.netty.NettyConfig.LeakDetectionLevel
import zio.http.Server.RequestStreaming
import zio.http.Routes
import java.io.File
import zio.http.Path
import com.augustnagro.magnum.magzio.* 

object Main extends zio.ZIOAppDefault:
    override val bootstrap =
        val logFormat = LogFormat.colored |-| LogFormat.spans
        Runtime.removeDefaultLoggers ++ Runtime.addLogger(CustomLogger.basicColoredLogger(LogLevel.Info)) // >>> SLF4J.slf4j(logFormat)

    type ClipperzBaseEnvironment =
        PRNG & SessionManager & TollManager & UserManager & BlobManager & OneTimeShareManager & SrpManager

    type ClipperzEnvironment = ClipperzBaseEnvironment | ClipperzBaseEnvironment & Transactor

    type ClipperzHttpApp = Routes[
        ClipperzEnvironment
    ,   Nothing
    ]

    val clipperzBackend: ClipperzHttpApp = (
    // val clipperzBackend = (
            usersApi        @@ hashcash(ChallengeType.REGISTER, ChallengeType.CONNECT)
        ++  loginApi        @@ hashcash(ChallengeType.CONNECT,  ChallengeType.MESSAGE)
        ++  logoutApi
        ++  blobsApi        @@ hashcash(ChallengeType.MESSAGE,  ChallengeType.MESSAGE)
        ++  oneTimeShareApi @@ hashcash(ChallengeType.SHARE,    ChallengeType.SHARE)
    )
    .handleErrorCauseZIO(customErrorHandler)
  
    val middlewares =
        Middleware.debug ++                                                         //  print debug info about request and response
        Middleware.timeout(20.seconds) ++                                           //  TODO: add timeout time to configuration file [fsolaroli - 10/01/2024]
        Middleware.requestLogging(logRequestBody = true, logResponseBody = true) ++ //  loggingMiddleware
        Middleware.serveDirectory(Path.root / "api" / "static", File("./target/output.webpack")) ++
        metrics()

    val completeClipperzBackend: ClipperzHttpApp = clipperzBackend @@ middlewares
    // val completeClipperzBackend = clipperzBackend @@ middlewares

    val keyValueStorageFolderDepth = 16

    val run = ZIOAppArgs.getArgs.flatMap ( args => {
        args(0) match {
            case "fileSystem" => {
                if args.length == 5
                then
                    val port = args(1).toInt
                    val blobBasePath         = FileSystem.default.getPath(args(2))
                    val userBasePath         = FileSystem.default.getPath(args(3))
                    val oneTimeShareBasePath = FileSystem.default.getPath(args(4))

                    val nThreads: Int = args.headOption.flatMap(x => Try(x.toInt).toOption).getOrElse(0)

                    val config        = Server.Config.default
                                            .responseCompression(Server.Config.ResponseCompressionConfig.default)
                                            .port(port)
                                            .enableRequestStreaming
                    val nettyConfig   = NettyConfig.default
                                            .leakDetection(LeakDetectionLevel.PARANOID)
                                            .maxThreads(nThreads)


                    // ( Files.createDirectories(blobBasePath) <&> 
                    //   Files.createDirectories(userBasePath) <&> 
                    //   Files.createDirectories(oneTimeShareBasePath)
                    // ) *>
                    Server
                        .install(completeClipperzBackend)
                        .flatMap(port =>
                            println("SERVER STARTED")
                                ZIO.logInfo(s"Server started on port ${port}")
                            *>  ZIO.never
                        )
                        .provide(
                            PRNG.live,
                            SessionManager.live(30.minutes), //TODO: add cache timeToLive to configuration file [fsolaroli - 10/01/2024]
                            TollManager.live,
                            UserManager.fileSystem(userBasePath, keyValueStorageFolderDepth, true),
                            BlobManager.fileSystem(blobBasePath, keyValueStorageFolderDepth, true),
                            OneTimeShareManager.fileSystem(oneTimeShareBasePath, keyValueStorageFolderDepth, true),
                            SrpManager.v6a(),

                            ZLayer.succeed(config),
                            ZLayer.succeed(nettyConfig),
                            Server.customized
                        ).tapError(e => ZIO.logError(s"Server failed with error: ${e.getMessage}"))
                else ZIO.logFatal("Not enough arguments")
            }
            case "db" => {
                if args.length == 3
                then
                    val port = args(1).toInt

                    val dataSourceConfig = new HikariConfig()
                    dataSourceConfig.setJdbcUrl("jdbc:sqlite:" + args(2) + "clipperzDb.db")
                    dataSourceConfig.setDriverClassName("org.sqlite.JDBC")
                    dataSourceConfig.setMaximumPoolSize(1) 
                
                    val dataSource = new HikariDataSource(dataSourceConfig)

                    val nThreads: Int = args.headOption.flatMap(x => Try(x.toInt).toOption).getOrElse(0)

                    val config        = Server.Config.default
                                            .responseCompression(Server.Config.ResponseCompressionConfig.default)
                                            .port(port)
                                            .enableRequestStreaming
                    val nettyConfig   = NettyConfig.default
                                            .leakDetection(LeakDetectionLevel.PARANOID)
                                            .maxThreads(nThreads)

                    // ( Files.createDirectories(args(2))
                    // ) *>
                    Server
                        .install(completeClipperzBackend)
                        .flatMap(port =>
                            println("SERVER STARTED")
                                ZIO.logInfo(s"Server started on port ${port}")
                            *>  ZIO.never
                        )
                        .provide(
                            PRNG.live,
                            SessionManager.live(30.minutes), //TODO: add cache timeToLive to configuration file [fsolaroli - 10/01/2024]
                            TollManager.live,
                            UserManager.sqlLite,
                            BlobManager.sqlLite(FileSystem.default.getPath("target/blobs")),
                            OneTimeShareManager.sqlLite,
                            SrpManager.v6a(),
                            Transactor.layer(dataSource),

                            ZLayer.succeed(config),
                            ZLayer.succeed(nettyConfig),
                            Server.customized
                        ).tapError(e => ZIO.logError(s"Server failed with error: ${e.getMessage}"))
                else ZIO.logFatal("Not enough arguments")
            }
            case _ => ZIO.logFatal("Not enough arguments")
        }
    })
