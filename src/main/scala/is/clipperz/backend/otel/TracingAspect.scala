package is.clipperz.backend.otel

import zio.ZIOAspect
import zio.{ URIO, ZIO }
import zio.telemetry.opentelemetry.tracing.Tracing
import zio.Trace
import zio.Supervisor
import zio.telemetry.opentelemetry.OpenTelemetry
import zio.telemetry.opentelemetry.common.Attribute
import io.opentelemetry.api.common.AttributeKey
import zio.http.*
import io.opentelemetry.api.common.Attributes
import io.opentelemetry.semconv.HttpAttributes

object TracingAspect:

    case class EndpointTracer():
        def wrap[R <: Tracing & PropagatorProvider, E, A](zio: ZIO[R, E, Response])(using Trace): ZIO[R, E, Response] =
            for {
                tracing     <- ZIO.service[Tracing]
                propagator  <- ZIO.service[PropagatorProvider]
                result      <- tracing.extractSpan(
                                    propagator = propagator.getTracePropagator(),
                                    carrier    = propagator.getIncomingCarrier(),
                                    spanName   = s"handler"
                                )(zio)
            } yield result

    extension [R <: Tracing & PropagatorProvider, E, A](zio: ZIO[R, E, Response])
        def @@(tracer: EndpointTracer)(using Trace): ZIO[R, E, Response] =
            tracer.wrap(zio)


    case class MethodTracer(spanName: String):
        def wrap[R <: Tracing, E, A](zio: ZIO[R, E, A])(using Trace): ZIO[R, E, A] =
            ZIO.serviceWithZIO[Tracing] { tracing =>
                tracing.span(spanName)(zio)
        }

    extension [R <: Tracing, E, A](zio: ZIO[R, E, A])
        def @@(tracer: MethodTracer)(using Trace): ZIO[R, E, A] =
            tracer.wrap(zio)
