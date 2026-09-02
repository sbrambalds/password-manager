package is.clipperz.backend.otel

import zio.*
import io.opentelemetry.sdk.metrics.SdkMeterProvider
import io.opentelemetry.sdk.metrics.`export`.PeriodicMetricReader
import io.opentelemetry.sdk.resources.Resource
import io.opentelemetry.api.common.Attributes
import io.opentelemetry.semconv.ResourceAttributes
import io.opentelemetry.exporter.otlp.metrics.OtlpGrpcMetricExporter
import io.opentelemetry.semconv.ServiceAttributes

object MeterProvider:

  def otlpGrpc(resourceName: String): RIO[Scope, SdkMeterProvider] =
    for {
        meterExporter   <- ZIO.fromAutoCloseable(ZIO.succeed(OtlpGrpcMetricExporter.builder().setEndpoint("http://" + sys.env.getOrElse("OTEL_EXPORTER_OTLP_ENDPOINT", "localhost:4317")).build()))
        metricReader    <- ZIO.fromAutoCloseable(ZIO.succeed(PeriodicMetricReader.builder(meterExporter).setInterval(30.seconds).build()))
        meterProvider   <-
            ZIO.fromAutoCloseable(
                ZIO.succeed(
                    SdkMeterProvider
                    .builder()
                    .registerMetricReader(metricReader)
                    .setResource(Resource.create(Attributes.of(ServiceAttributes.SERVICE_NAME, resourceName)))
                    .build()
                )
            )
    } yield meterProvider

