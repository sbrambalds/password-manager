package is.clipperz.backend

import is.clipperz.backend.apis.{ blobsApi, loginApi, logoutApi, staticApi, usersApi, oneTimeShareApi }
import is.clipperz.backend.functions.{ customErrorHandler }
import is.clipperz.backend.middleware.{ hashcash, metrics }
import is.clipperz.backend.services.{ BlobManager, PRNG, SessionManager, SrpManager, TollManager, UserManager, OneTimeShareManager }
import is.clipperz.backend.services.ChallengeType

import zio.nio.file.{ Files, FileSystem }

import scala.util.Try

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

object Main extends zio.ZIOAppDefault:
    override val bootstrap =
        val logFormat = LogFormat.colored |-| LogFormat.spans
        Runtime.removeDefaultLoggers ++ Runtime.addLogger(CustomLogger.basicColoredLogger(LogLevel.Info)) // >>> SLF4J.slf4j(logFormat)

    type ClipperzEnvironment =
        PRNG & SessionManager & TollManager & UserManager & BlobManager & OneTimeShareManager & SrpManager

    type ClipperzHttpApp = Routes[
        ClipperzEnvironment
    ,   Nothing
    ]

    val clipperzBackend: ClipperzHttpApp = (
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

    val keyValueStorageFolderDepth = 16

    val run = ZIOAppArgs.getArgs.flatMap { args =>
        if args.length == 4
        then
            val blobBasePath         = FileSystem.default.getPath(args(0))
            val userBasePath         = FileSystem.default.getPath(args(1))
            val oneTimeShareBasePath = FileSystem.default.getPath(args(2))
            val port = args(3).toInt

            val MB = 1024 * 1024

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
                )

        else ZIO.logFatal("Not enough arguments")
    }
