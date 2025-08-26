package is.clipperz.backend.apis

import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.LogAspect
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.services.SessionManager

import java.util

import zio.{ ZIO, Cause }
import zio.http.{ Method, Path, Response, Request, Routes, handler }
import is.clipperz.backend.otel.PropagatorProvider
import zio.telemetry.opentelemetry.tracing.Tracing
import is.clipperz.backend.otel.TracingAspect.EndpointTracer

val logoutApi: Routes[SessionManager & Tracing & PropagatorProvider, Throwable] = Routes(
    Method.POST / "api" / "logout" -> handler: (request: Request) =>
        (for {
            sessionManager <- ZIO.service[SessionManager]
            _              <- sessionManager.deleteSession(request)
        } yield Response.ok) @@ LogAspect.logAnnotateRequestData(request)
        @@ LogAspect.logAnnotateRequestData(request)
        @@ EndpointTracer()
)