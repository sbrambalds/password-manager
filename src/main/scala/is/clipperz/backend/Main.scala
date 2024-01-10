package is.clipperz.backend

import java.nio.file.FileSystems
import scala.util.Try
import zio.{ Cause, ZIO, Scope, Task, ZIOAppArgs, ZIOAppDefault, LogLevel, Runtime, ZLayer }
import zio.durationInt
import zio.logging.LogFormat
import zio.logging.backend.SLF4J
import zio.metrics.connectors.datadog
import zio.metrics.connectors.MetricsConfig

import zio.http.{
  Header,
  Headers,
  HttpApp,
  Method,
  Path,
  Request,
  Response,
  Status,
}
import zio.http.{ Server }
import zio.http.netty.{ EventLoopGroups, NettyConfig }
import zio.http.netty.NettyConfig.LeakDetectionLevel

import is.clipperz.backend.apis.{ blobsApi, loginApi, logoutApi, staticApi, usersApi, oneTimeShareApi }
import is.clipperz.backend.functions.{ customErrorHandler }
import is.clipperz.backend.middleware.{ hashcash, metrics }
import is.clipperz.backend.services.{ BlobArchive, PRNG, SessionManager, SrpManager, TollManager, UserArchive, OneTimeShareArchive }
import zio.http.Server.RequestStreaming
import zio.http.Routes
import zio.http.endpoint.Endpoint
import zio.http.endpoint.openapi.OpenAPI
import zio.http.endpoint.openapi.OpenAPIGen
import zio.http.Status.NotFound
import is.clipperz.backend.exceptions.*
import java.time.format.DateTimeParseException
import is.clipperz.backend.services.ChallengeType
import java.time.Duration

object Main extends zio.ZIOAppDefault:
  override val bootstrap =
    val logFormat = LogFormat.colored |-| LogFormat.spans
    Runtime.removeDefaultLoggers ++ Runtime.addLogger(CustomLogger.basicColoredLogger(LogLevel.Info)) // >>> SLF4J.slf4j(logFormat)

  type ClipperzEnvironment =
    PRNG & SessionManager & TollManager & UserArchive & BlobArchive & OneTimeShareArchive & SrpManager

  type ClipperzHttpApp = HttpApp[
    ClipperzEnvironment
  ]

  val clipperzBackend: ClipperzHttpApp = (
        usersApi        @@ hashcash(ChallengeType.REGISTER, ChallengeType.CONNECT)
    ++  loginApi        @@ hashcash(ChallengeType.CONNECT,  ChallengeType.MESSAGE)
    ++  logoutApi
    ++  blobsApi        @@ hashcash(ChallengeType.MESSAGE,  ChallengeType.MESSAGE)
    ++  oneTimeShareApi @@ hashcash(ChallengeType.SHARE,    ChallengeType.SHARE)
    ++  staticApi
    ).handleErrorCauseZIO(customErrorHandler)
     .toHttpApp
  
  val completeClipperzBackend: ClipperzHttpApp = clipperzBackend @@ metrics()

  val run = ZIOAppArgs.getArgs.flatMap { args =>
    if args.length == 4 then
      val blobBasePath = FileSystems.getDefault().nn.getPath(args(0)).nn
      val userBasePath = FileSystems.getDefault().nn.getPath(args(1)).nn
      val oneTimeShareBasePath = FileSystems.getDefault().nn.getPath(args(2)).nn
      val port = args(3).toInt

      val MB = 1024 * 1024

      val nThreads: Int = args.headOption.flatMap(x => Try(x.toInt).toOption).getOrElse(0)

      val config        = Server.Config.default
                            .port(port)
                            .requestStreaming(RequestStreaming.Enabled)
      val nettyConfig   = NettyConfig.default
                            .leakDetection(LeakDetectionLevel.PARANOID)
                            .maxThreads(nThreads)

      blobBasePath.toFile().nn.mkdirs()
      userBasePath.toFile().nn.mkdirs()
      oneTimeShareBasePath.toFile().nn.mkdirs()

      Server
        .install(completeClipperzBackend)
        .flatMap(port =>
             ZIO.logInfo(s"Server started on port ${port}")
          *> ZIO.never
        )
        .provide(
          PRNG.live,
          SessionManager.live(30.minutes),
          TollManager.live,
          UserArchive.fs(userBasePath, 2, true),
          BlobArchive.fs(blobBasePath, 2, true),
          OneTimeShareArchive.fs(oneTimeShareBasePath, 2, true),
          SrpManager.v6a(),
          
          ZLayer.succeed(config),
          ZLayer.succeed(nettyConfig),
          Server.customized,
          
          datadog.datadogLayer,
          ZLayer.succeed(datadog.DatadogConfig("dd-agent", 8125)),
          ZLayer.succeed(MetricsConfig(100.millis)),
        )

    else ZIO.logFatal("Not enough arguments")
  }
