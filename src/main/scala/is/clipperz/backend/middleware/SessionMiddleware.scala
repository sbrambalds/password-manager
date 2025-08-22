package is.clipperz.backend.middleware

import is.clipperz.backend.data.HexString
import is.clipperz.backend.functions.{ customErrorHandler, customMapError, fromString }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.services.{ ChallengeType, Session, SessionManager, TollManager, TollChallenge }

import zio.{ ZIO, Task }
import zio.json.EncoderOps
import zio.http.{ HandlerAspect, Headers, Middleware, Request, Response, Status }
import zio.http.Status.{ InternalServerError, Unauthorized }
import zio.telemetry.opentelemetry.tracing.Tracing
import is.clipperz.backend.otel.PropagatorProvider

type SessionMiddleware = HandlerAspect[Tracing & PropagatorProvider & SessionManager, Any]

def authorizedMiddleware(cExtractor: (Request) => Task[String]): SessionMiddleware =
    Middleware.ifRequestThenElseZIO(req =>
        ZIO.service[Tracing].zip(ZIO.service[PropagatorProvider]).flatMap((tracing, propagatorProvider) =>
            (for {
                sessionManager <- ZIO.service[SessionManager]
                session        <- sessionManager.getSession(req)
                c              <- cExtractor(req)
            } yield sessionManager.verifySessionUser(c, session)
            ).mapError(customMapError)
            @@ tracing.aspects.extractSpan(
                propagatorProvider.getTracePropagator()
            ,   propagatorProvider.getIncomingCarrier()
            ,   s"authorizedMiddleware"
            )
        )
    )(
        Middleware.identity
    ,   Middleware.fail(Response.status(Unauthorized))
    )