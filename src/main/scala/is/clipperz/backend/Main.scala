package is.clipperz.backend

import is.clipperz.backend.apis.{ blobsApi, loginApi, logoutApi, staticApi, usersApi, oneTimeShareApi }
import is.clipperz.backend.functions.{ customErrorHandler }
import is.clipperz.backend.middleware.{ hashcash }
import is.clipperz.backend.services.{ BlobManager, PRNG, SessionManager, SrpManager, TollManager, UserManager, OneTimeShareManager, Metric }
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
import zio.metrics.Metrics

object Main extends zio.ZIOAppDefault:
    override val bootstrap =
        val logFormat = LogFormat.colored |-| LogFormat.spans
        Runtime.removeDefaultLoggers ++ Runtime.addLogger(CustomLogger.basicColoredLogger(LogLevel.Info)) // >>> SLF4J.slf4j(logFormat)

    type InstrumentationEnvironment = 
        Tracing & io.opentelemetry.api.OpenTelemetry & PropagatorProvider & zio.telemetry.opentelemetry.context.ContextStorage

    type ClipperzEnvironment =
        SessionManager & TollManager & UserManager & BlobManager & OneTimeShareManager & SrpManager & InstrumentationEnvironment

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

        val port      = args.lift(1).flatMap(s => Try(s.toInt).toOption)
                            .getOrElse(sys.error("Missing or invalid port argument"))

        val nThreads  = args.headOption.flatMap(s => Try(s.toInt).toOption).getOrElse(0)

        // Server config
        val config = Server.Config.default
            .responseCompression(Server.Config.ResponseCompressionConfig.default)
            .port(port)
            .enableRequestStreaming

        val nettyConfig = NettyConfig.default
            .leakDetection(LeakDetectionLevel.PARANOID)
            .maxThreads(nThreads)

        val configLayer     = (ZLayer.succeed(config) ++ ZLayer.succeed(nettyConfig)) >>> Server.customized
        val otelCore        = OtelSdk.custom(sourceName) ++ OpenTelemetry.contextZIO ++ PropagatorProvider.live()
        val servicesLayer   = PRNG.live >>> (SrpManager.v6a() ++ SessionManager.live(30.minutes) ++ TollManager.live)

        val otelTracing     = otelCore >>> (OpenTelemetry.tracing(instrumentationScopeName))
        val otelLogging     = otelCore >>> (OpenTelemetry.logging(instrumentationScopeName))
        val otelMetrics     = otelCore >>> (OpenTelemetry.metrics(instrumentationScopeName))
        val clipperzLayer   = otelCore ++ servicesLayer ++ configLayer

        val storageLayerValue = args(0) match
            case "fileSystem" =>
                if args.length == 5
                then
                    val blobBasePath         = FileSystem.default.getPath(args(2))
                    val userBasePath         = FileSystem.default.getPath(args(3))
                    val oneTimeShareBasePath = FileSystem.default.getPath(args(4))

                    Right(
                        UserManager.fileSystem(userBasePath, keyValueStorageFolderDepth, true) ++
                        BlobManager.fileSystem(blobBasePath, keyValueStorageFolderDepth, true) ++
                        OneTimeShareManager.fileSystem(oneTimeShareBasePath, keyValueStorageFolderDepth, true)
                    )

                else Left(ZIO.logFatal("Not enough arguments"))

            case "db" =>
                if args.length == 3
                then
                    val blobPath = FileSystem.default.getPath("target/blobs")

                    val dataSourceConfig = new HikariConfig()
                    dataSourceConfig.setJdbcUrl("jdbc:sqlite:" + args(2) + "clipperzDb.db")
                    dataSourceConfig.setDriverClassName("org.sqlite.JDBC")
                    dataSourceConfig.setMaximumPoolSize(1) 
                
                    val dataSource = new HikariDataSource(dataSourceConfig)
                    val transactor = Transactor.layer(dataSource)

                    Right(
                        UserManager.sqlLite(transactor) ++
                        BlobManager.sqlLite(blobPath, transactor) ++
                        OneTimeShareManager.sqlLite(transactor) 
                    )

                else Left(ZIO.logFatal("Not enough arguments"))

            case "s3" =>
                if args.length == 3
                then
                    val blobPath = FileSystem.default.getPath("target/blobs")
    
                    val s3 = zio.s3
                                .live(
                                    Region.EU_CENTRAL_1,
                                    AwsBasicCredentials.create("TESTKEY", "TESTSECRET"),
                                    Some(URI.create("http://127.0.0.1:9000")),
                                    forcePathStyle = Some(true)
                                )
                    Right(
                        s3 >>> (
                            UserManager.minIO(s3) ++
                            BlobManager.minIO(blobPath, s3) ++
                            OneTimeShareManager.minIO(s3)
                        )
                    )

                else Left(ZIO.logFatal("Not enough arguments"))

            case _ => Left(ZIO.logFatal("Error during running"))

        storageLayerValue match
            case Right(storageLayer) => 
                Server
                    .serve(completeClipperzBackend)
                    .provide(
                        clipperzLayer,
                        storageLayer,
                        otelTracing
                    ).provide(
                        otelLogging
                    ).provide(
                        otelMetrics,
                        OpenTelemetry.zioMetrics
                    ).tapError(e => ZIO.logError(s"Server failed with error: ${e.getMessage}"))
            case Left(error) => error
    })
