package is.clipperz.backend.otel

import zio.telemetry.opentelemetry.tracing.propagation.TraceContextPropagator
import zio.ZLayer
import zio.telemetry.opentelemetry.context.OutgoingContextCarrier
import zio.ZIO
import scala.collection.mutable
import zio.telemetry.opentelemetry.context.IncomingContextCarrier

trait PropagatorProvider:
    def getTracePropagator(): TraceContextPropagator
    def getOutgoingCarrier(): OutgoingContextCarrier[scala.collection.mutable.Map[String, String]]
    def getIncomingCarrier(): IncomingContextCarrier[scala.collection.mutable.Map[String, String]]

object PropagatorProvider:
    case class DefaultPropagatorProvider() extends PropagatorProvider:
        val tracePropagator = TraceContextPropagator.default
        val kernel          = mutable.Map.empty[String, String]
        val outgoingCarrier = OutgoingContextCarrier.default(kernel)
        val incomingCarrier = IncomingContextCarrier.default(kernel)

        override def getTracePropagator() = this.tracePropagator

        override def getOutgoingCarrier() = this.outgoingCarrier

        override def getIncomingCarrier() = this.incomingCarrier


    def live(): ZLayer[Any, Nothing, PropagatorProvider] =
        ZLayer(ZIO.succeed((new DefaultPropagatorProvider())));
