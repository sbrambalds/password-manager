package is.clipperz.backend

import is.clipperz.backend.apis.{ blobsApi, loginApi, logoutApi, staticApi, usersApi, oneTimeShareApi }
import is.clipperz.backend.functions.{ customErrorHandler }
import is.clipperz.backend.middleware.{ hashcash }
import is.clipperz.backend.services.{ BlobManager, PRNG, SessionManager, SrpManager, TollManager, UserManager, OneTimeShareManager }
import is.clipperz.backend.services.ChallengeType
import is.clipperz.backend.otel.*

import software.amazon.awssdk.auth.credentials.AwsBasicCredentials
import software.amazon.awssdk.regions.Region

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
import com.augustnagro.magnum.magzio.* 
import zio.http.Path
import zio.telemetry.opentelemetry.OpenTelemetry
import is.clipperz.backend.otel.OtelSdk
import zio.telemetry.opentelemetry.tracing.Tracing
import is.clipperz.backend.middleware.trace
import java.net.URI
import zio.s3.*
import software.amazon.awssdk.services.s3.S3AsyncClient
import zio.s3.S3
import zio.s3.S3Settings
import zio.s3.S3Region

object Main extends zio.ZIOAppDefault:
    override val bootstrap =
        val logFormat = LogFormat.colored |-| LogFormat.spans
        Runtime.removeDefaultLoggers ++ Runtime.addLogger(CustomLogger.basicColoredLogger(LogLevel.Info)) // >>> SLF4J.slf4j(logFormat)

    type InstrumentationEnvironment = 
        Tracing & io.opentelemetry.api.OpenTelemetry & PropagatorProvider & zio.telemetry.opentelemetry.context.ContextStorage

    type ClipperzEnvironment =
        PRNG & SessionManager & TollManager & UserManager & BlobManager & OneTimeShareManager & SrpManager & InstrumentationEnvironment

    type ClipperzHttpApp = Routes[
        ClipperzEnvironment
    ,   Nothing 
    ]

    val sourceName = "is.clipperz.epsilon"
    val instrumentationScopeName = "scala-backend"

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
        Middleware.metrics() ++
        trace(instrumentationScopeName)

    val completeClipperzBackend: ClipperzHttpApp = clipperzBackend @@ middlewares

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


                    ( 
                        Files.createDirectories(blobBasePath) <*>
                        Files.createDirectories(userBasePath) <*>
                        Files.createDirectories(oneTimeShareBasePath)
                    ) *>
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
                                                        
                            OtelSdk.custom(sourceName),
                            OpenTelemetry.metrics(instrumentationScopeName),
                            OpenTelemetry.logging(instrumentationScopeName),
                            OpenTelemetry.tracing(instrumentationScopeName),
                            OpenTelemetry.zioMetrics,
                            OpenTelemetry.contextZIO,
                            PropagatorProvider.live(),
                            zio.metrics.jvm.DefaultJvmMetrics.liveV2.unit,

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
                    val transactor = Transactor.layer(dataSource)

                    val nThreads: Int = args.headOption.flatMap(x => Try(x.toInt).toOption).getOrElse(0)

                    val config        = Server.Config.default
                                            .responseCompression(Server.Config.ResponseCompressionConfig.default)
                                            .port(port)
                                            .enableRequestStreaming
                    val nettyConfig   = NettyConfig.default
                                            .leakDetection(LeakDetectionLevel.PARANOID)
                                            .maxThreads(nThreads)
                    Server
                        .install(completeClipperzBackend)
                        .flatMap(port =>
                            ZIO.logInfo("SERVER STARTED") *>
                                ZIO.logInfo(s"Server started on port ${port}")
                            *>  ZIO.never
                        )
                        .provide(
                            PRNG.live,
                            SessionManager.live(30.minutes), //TODO: add cache timeToLive to configuration file [fsolaroli - 10/01/2024]
                            TollManager.live,
                            UserManager.sqlLite(transactor),
                            BlobManager.sqlLite(FileSystem.default.getPath("target/blobs"), transactor),
                            OneTimeShareManager.sqlLite(transactor),
                            SrpManager.v6a(),

                            ZLayer.succeed(config),
                            ZLayer.succeed(nettyConfig),
                            Server.customized,
                            OtelSdk.custom(sourceName),
                            OpenTelemetry.metrics(instrumentationScopeName),
                            OpenTelemetry.logging(instrumentationScopeName, LogLevel.All),
                            OpenTelemetry.tracing(instrumentationScopeName),
                            OpenTelemetry.zioMetrics,
                            OpenTelemetry.contextZIO,
                            PropagatorProvider.live(),
                            zio.metrics.jvm.DefaultJvmMetrics.liveV2.unit
                        ).tapError(e => ZIO.logError(s"Server failed with error: ${e.getMessage}"))
                else ZIO.logFatal("Not enough arguments")
            }
            case "s3" => {
                val port = args(1).toInt

                val nThreads: Int = args.headOption.flatMap(x => Try(x.toInt).toOption).getOrElse(0)

                val config        = Server.Config.default
                                        .responseCompression(Server.Config.ResponseCompressionConfig.default)
                                        .port(port)
                                        .enableRequestStreaming
                val nettyConfig   = NettyConfig.default
                                        .leakDetection(LeakDetectionLevel.PARANOID)
                                        .maxThreads(nThreads)
 
                val s3 = zio.s3
                            .live(
                                Region.EU_CENTRAL_1,
                                AwsBasicCredentials.create("TESTKEY", "TESTSECRET"),
                                Some(URI.create("http://127.0.0.1:9000")),
                                forcePathStyle = Some(true)
                            )

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
                        s3,
                        UserManager.minIO(s3, keyValueStorageFolderDepth),
                        BlobManager.minIO(FileSystem.default.getPath("target/blobs"), s3, keyValueStorageFolderDepth),
                        OneTimeShareManager.minIO(s3, keyValueStorageFolderDepth),
                        SrpManager.v6a(),
                                                    
                        OtelSdk.custom(sourceName),
                        OpenTelemetry.metrics(instrumentationScopeName),
                        OpenTelemetry.logging(instrumentationScopeName),
                        OpenTelemetry.tracing(instrumentationScopeName),
                        OpenTelemetry.zioMetrics,
                        OpenTelemetry.contextZIO,
                        PropagatorProvider.live(),
                        zio.metrics.jvm.DefaultJvmMetrics.liveV2.unit,

                        ZLayer.succeed(config),
                        ZLayer.succeed(nettyConfig),
                        Server.customized
                    ).tapError(e => ZIO.logError(s"Server failed with error: ${e.getMessage}"))
            }
            case _ => ZIO.logFatal("Error during running")
        }
    })
