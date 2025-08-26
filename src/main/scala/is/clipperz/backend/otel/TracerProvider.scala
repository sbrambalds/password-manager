package is.clipperz.backend.otel

import io.opentelemetry.api.common.Attributes
import io.opentelemetry.exporter.otlp.trace.OtlpGrpcSpanExporter
import io.opentelemetry.sdk.resources.Resource
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.sdk.trace.`export`.SimpleSpanProcessor
import zio.*
import io.opentelemetry.exporter.otlp.http.trace.OtlpHttpSpanExporter
import io.opentelemetry.semconv.ServiceAttributes
import io.opentelemetry.*
import io.opentelemetry.context.ContextStorage
import io.opentelemetry.api.trace.Tracer
import io.opentelemetry.context.propagation.ContextPropagators
import io.opentelemetry.api.OpenTelemetry
import io.opentelemetry.sdk.trace.`export`.BatchSpanProcessor
import io.opentelemetry.semconv.ResourceAttributes

object TracerProvider:

    val signozEndpoint =  "https://localhost:4317"

    def default(resourceName: String): RIO[Scope, SdkTracerProvider] =
        for {
            spanExporter   <- ZIO.fromAutoCloseable(ZIO.succeed(OtlpGrpcSpanExporter.builder().build()))
            spanProcessor  <- ZIO.fromAutoCloseable(ZIO.succeed(SimpleSpanProcessor.create(spanExporter)))
            tracerProvider <-
                ZIO.fromAutoCloseable(
                    ZIO.succeed(
                        SdkTracerProvider
                        .builder()
                        .setResource(Resource.create(Attributes.of(ServiceAttributes.SERVICE_NAME, resourceName)))
                        .addSpanProcessor(spanProcessor)
                        .build()
                    )
                )
        } yield tracerProvider

    def otlpGrpc(resourceName: String): RIO[Scope, SdkTracerProvider] =
        for {
            spanExporter <- ZIO.attempt:
                OtlpGrpcSpanExporter.builder()
                    .setEndpoint(signozEndpoint)
                    .build()
            spanProcessor  <- ZIO.fromAutoCloseable(ZIO.succeed(SimpleSpanProcessor.create(spanExporter)))
            tracerProvider <-
                ZIO.fromAutoCloseable(
                    ZIO.succeed(
                        SdkTracerProvider.builder()
                            .addSpanProcessor(BatchSpanProcessor.builder(spanExporter).build())
                            .setResource(Resource.create(
                            Attributes.of(ServiceAttributes.SERVICE_NAME, resourceName)
                            ))
                        .build()
                    )
                )
        } yield tracerProvider
