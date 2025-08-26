package is.clipperz.backend.otel

import zio.*
import zio.telemetry.opentelemetry.OpenTelemetry
import io.opentelemetry.sdk.OpenTelemetrySdk
import io.opentelemetry.api

object OtelSdk:

    def custom(resourceName: String): TaskLayer[api.OpenTelemetry] =
        OpenTelemetry.custom(
            for {
                tracerProvider <- TracerProvider.default(resourceName)
                meterProvider  <- MeterProvider .otlpGrpc(resourceName)
                loggerProvider <- LoggerProvider.otlpGrpc(resourceName)
                openTelemetry  <- ZIO.fromAutoCloseable(
                                    ZIO.succeed(
                                    OpenTelemetrySdk
                                        .builder()
                                        .setTracerProvider(tracerProvider)
                                        .setMeterProvider (meterProvider)
                                        .setLoggerProvider(loggerProvider)
                                        .build
                                    )
                                )
            } yield openTelemetry
        )


    val test: TaskLayer[api.OpenTelemetry] = 
        OpenTelemetry.custom(
            ZIO.fromAutoCloseable(
                ZIO.succeed(
                OpenTelemetrySdk
                    .builder()
                    .build
                )
            )
        )
