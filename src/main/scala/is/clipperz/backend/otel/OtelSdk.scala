package is.clipperz.backend.otel

import zio.*
import zio.telemetry.opentelemetry.OpenTelemetry
import io.opentelemetry.sdk.OpenTelemetrySdk
import io.opentelemetry.api

object OtelSdk:

    def custom(resourceName: String, backendType: String): TaskLayer[api.OpenTelemetry] =
        OpenTelemetry.custom(
            for {
                tracerProvider <- TracerProvider.otlpGrpc(resourceName, backendType)
                meterProvider  <- MeterProvider .otlpGrpc(resourceName, backendType)
                loggerProvider <- LoggerProvider.otlpGrpc(resourceName, backendType)
                openTelemetry  <- ZIO.succeed(
                                    OpenTelemetrySdk
                                        .builder()
                                        .setTracerProvider(tracerProvider)
                                        .setMeterProvider (meterProvider)
                                        .setLoggerProvider(loggerProvider)
                                        .build
                                    )
            } yield openTelemetry
        )


    val test: TaskLayer[api.OpenTelemetry] = 
        OpenTelemetry.custom(
            ZIO.succeed(
            OpenTelemetrySdk
                .builder()
                .build
            )
        )
