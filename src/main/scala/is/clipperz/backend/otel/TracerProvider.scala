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
import zio.http.endpoint.openapi.JsonSchema.EnumValue.Str

object TracerProvider:

    def otlpGrpc(resourceName: String, backendType: String): RIO[Scope, SdkTracerProvider] =
        for {
            spanExporter   <- ZIO.fromAutoCloseable(ZIO.succeed(OtlpGrpcSpanExporter.builder().setEndpoint("http://" + sys.env.getOrElse("OTEL_EXPORTER_OTLP_ENDPOINT", "localhost:4317")).build()))
            spanProcessor  <- ZIO.fromAutoCloseable(ZIO.succeed(BatchSpanProcessor.builder(spanExporter).build()))
            tracerProvider <-
                ZIO.fromAutoCloseable(
                    ZIO.succeed(
                        SdkTracerProvider
                        .builder()
                        .setResource(Resource.create(Attributes.builder()
                            .put(ServiceAttributes.SERVICE_NAME, resourceName)
                            .put("backend.type", backendType)
                            .build()))
                        .addSpanProcessor(spanProcessor)
                        .build()
                    )
                )
        } yield tracerProvider
