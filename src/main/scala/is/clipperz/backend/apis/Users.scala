package is.clipperz.backend.apis

import is.clipperz.backend.data.{ HexString, Base }
import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.functions.{ fromStream }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.middleware.authorizedMiddleware
import is.clipperz.backend.otel.LogAspect
import is.clipperz.backend.services.{BlobManager, RequestUserCard, SessionManager, SignupData, UserManager, UserCard, remoteFromRequest}

import zio.{ ZIO, Cause }
import zio.http.{ Method, Response, Request, Routes, Status, handler, string }
import zio.json.EncoderOps
import zio.stream.ZStream
import is.clipperz.backend.services.CardsSignupData
import zio.telemetry.opentelemetry.tracing.Tracing
import is.clipperz.backend.otel.PropagatorProvider
import is.clipperz.backend.otel.TracingAspect.EndpointTracer

val usersApi: Routes[BlobManager & UserManager & SessionManager & Tracing & PropagatorProvider, Throwable] = Routes(
    Method.POST / "api" / "users" / string("c") -> handler: (c: String, request: Request) =>
        ZIO
        .service[UserManager]
        .zip(ZIO.service[BlobManager])
        .zip(ZIO.service[SessionManager])
        .zip(ZIO.succeed(request.body.asStream))
        .flatMap((userManager, blobManager, sessionManager, content) =>
        userManager
            .getUser(HexString(c))
            .flatMap(optionalUser =>
            optionalUser match
                case Some(_) => ZIO.fail(new ConflictualRequestException("User already exists"))
                case None    => ZIO.succeed(())
            )
            .flatMap(_ =>
            fromStream[SignupData](content)
                .flatMap { signupData =>
                if HexString(c) == signupData.user.c then
                    (   userManager.saveUser(remoteFromRequest(signupData.user), false)
                    <&> blobManager.saveBlob(signupData.indexCardReference, signupData.indexCardIdentifier, ZStream.fromIterable(signupData.indexCardContent.toByteArray))
                    <&> blobManager.saveBlob(signupData.userInfoReference,  signupData.userInfoIdentifier,  ZStream.fromIterable(signupData.userInfoContent.toByteArray))
                    <&> ZIO.foreach(signupData.cards) {
                            cardsSignupData => blobManager.saveBlob(cardsSignupData.cardReference, cardsSignupData.cardIdentifier, ZStream.fromIterable(cardsSignupData.cardContent.toByteArray))
                        }
                    )
                    .parallelErrors
                    .foldZIO(
                        err => ZIO.fail(new Exception(s"${err}")),
                        result => ZIO.succeed(result),
                    )
                else ZIO.fail(new BadRequestException("c in request path differs from c in request body "))
                }
            )
        )
        .map(results => Response.text(results._1.toString))
        @@ LogAspect.logAnnotateRequestData(request)
        @@ EndpointTracer() 
) ++ 
Routes(
    Method.PUT / "api"  / "users" / string("c") -> handler: (c: String, request: Request) =>
        ZIO
        .service[UserManager]
        .zip(ZIO.service[BlobManager])
        .zip(ZIO.service[SessionManager])
        .zip(ZIO.succeed(request.body.asStream))
        .flatMap((userManager, blobManager, sessionManager, content) =>
            userManager
            .getUser(HexString(c))
            .flatMap(optionalUser =>
            optionalUser match
                case Some(u) => ZIO.succeed(u)
                case None => ZIO.fail(new ResourceNotFoundException(s"user ${c} does not exist"))
            )
            .flatMap(currentUser =>
            fromStream[RequestUserCard](content)
                .flatMap { userCard =>
                    userManager
                        .getUser(HexString(userCard.c.toString))
                        .flatMap(optionalUser =>
                        optionalUser match
                            case Some(_) => ZIO.fail(new ConflictualRequestException("User already exists"))
                            case None    => ZIO.succeed(())
                        )
                    *>
                    (if userCard.originMasterKey.contains(currentUser.masterKey(0)) 
                    then
                        (userManager.saveUser(remoteFromRequest(userCard), true))
                        <&>
                        (sessionManager.updateSession(request, userCard.c.toString()))
                        <&>
                        (userManager.deleteUser(HexString(c)))
                    else
                        ZIO.fail(new BadRequestException("origin does not match"))
                    )
                }
            )
        )
        .map(_ => Response.ok)
        @@ LogAspect.logAnnotateRequestData(request)
        @@ EndpointTracer()
,  
    Method.PATCH / "api" / "users" / string("c") -> handler: (c: String, request: Request) =>
        ZIO
        .service[UserManager]
        .zip(ZIO.service[BlobManager])
        .zip(ZIO.succeed(request.body.asStream))
        .flatMap((userManager, blobManager, content) =>
            userManager
            .getUser(HexString(c))
            .flatMap(optionalUser =>
            optionalUser match
                case Some(u) => ZIO.succeed(u)
                case None => ZIO.fail(new ResourceNotFoundException(s"user ${c} does not exist"))
            )
            .flatMap(currentUser =>
                fromStream[UserCard](content)
                    .flatMap { userCard =>
                    if userCard.originMasterKey == currentUser.masterKey(0) then
                        userManager.saveUser(currentUser.copy(masterKey = userCard.masterKey), true)
                    else
                        ZIO.fail(new BadRequestException("origin does not match"))
                    }
            )
        )
        .map(_ => Response.ok)
        @@ LogAspect.logAnnotateRequestData(request)
        @@ EndpointTracer()
,
    Method.GET / "api" / "users" / string("c") -> handler: (c: String, request: Request) =>
        (for {
            userManager  <- ZIO.service[UserManager]
            optionalUser <- userManager.getUser(HexString(c))
        } yield (optionalUser match
            case None       => Response(status = Status.NotFound)
            case Some(card) => Response.json(card.masterKey.toJson)
        )) @@ LogAspect.logAnnotateRequestData(request)
        @@ EndpointTracer()
,
    Method.DELETE / "api" / "users" / string("c") -> handler: (c: String, request: Request) =>
        (for {
            userManager    <- ZIO.service[UserManager]
            sessionManager <- ZIO.service[SessionManager]
            _              <- userManager.deleteUser(HexString(c))
            _              <- sessionManager.deleteSession(request)
        } yield Response.text(c)) 
        @@ LogAspect.logAnnotateRequestData(request)
        @@ EndpointTracer()
) @@ authorizedMiddleware(req => ZIO.attempt(req.path.segments.last))