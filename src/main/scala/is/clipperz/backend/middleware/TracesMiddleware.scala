package is.clipperz.backend.middleware

import io.opentelemetry.api.baggage.Baggage
import io.opentelemetry.api.baggage.propagation.W3CBaggagePropagator
import io.opentelemetry.context.propagation.TextMapGetter

import zio.Trace
import zio.ZIO
import zio.ZIOAspect
import zio.http.*
import zio.telemetry.opentelemetry.tracing.Tracing

import java.lang
import scala.jdk.CollectionConverters.IterableHasAsJava
import io.opentelemetry.api.common.Attributes
import io.opentelemetry.semconv.HttpAttributes
import io.opentelemetry.api.trace.TraceId
import io.opentelemetry.api.trace.Span
import io.opentelemetry.semconv.UrlAttributes
import io.opentelemetry.semconv.JvmAttributes
import io.opentelemetry.sdk.internal.AttributesMap
import io.opentelemetry.api.trace.Tracer
import io.opentelemetry.api.trace.TracerProvider
import io.opentelemetry.sdk.trace.SdkTracerProvider
import io.opentelemetry.api.OpenTelemetry
import io.opentelemetry.context.ContextStorage
import zio.ZLayer
import java.util.concurrent.TimeUnit
import zio.Clock
import io.opentelemetry.api.trace.StatusCode
import is.clipperz.backend.otel.PropagatorProvider
import io.opentelemetry.context.Context

def trace(instrumentationScopeName: String): Middleware[io.opentelemetry.api.OpenTelemetry & PropagatorProvider] =
	Middleware.interceptHandlerStateful(
		Handler.fromFunctionZIO[Request] { request =>
			ZIO.service[io.opentelemetry.api.OpenTelemetry].zip(ZIO.service[PropagatorProvider]).flatMap((openTelemetry, propagatorProvider) => 
				for {
					tracer	<- 	ZIO.succeed(openTelemetry.getTracer(instrumentationScopeName))
					span 	<- 	ZIO.succeed(
									tracer
										.spanBuilder(s"${request.method} ${request.url.path}")
										.setAllAttributes(
											Attributes
												.builder()
												.put(HttpAttributes.HTTP_REQUEST_METHOD, request.method.toString)
												.put(HttpAttributes.HTTP_ROUTE,		     request.url.toString)
												.put("http.url.full", request.url.encode)
												.put("http.query", request.url.queryParams(",").toString)
												.build()
										)
										.setNoParent()
										.startSpan()
								)
					tracePropagator = propagatorProvider.getTracePropagator()
					outgoingCarrier = propagatorProvider.getOutgoingCarrier()
					_ = tracePropagator.instance.inject(Context.root().`with`(span), outgoingCarrier.kernel, outgoingCarrier)
				} yield(span, (request, ()))
			)
		}
	)(
		Handler.fromFunctionZIO[(Span, Response)] { case (span, response) =>
			for {
				_     <- ZIO.succeed(
							span
								.setStatus( if (response.status.isError) StatusCode.ERROR else StatusCode.OK)
								.setAttribute(HttpAttributes.HTTP_RESPONSE_STATUS_CODE, response.status.code.toLong)
								.end()
						)
			} yield (response)
		}
	)
