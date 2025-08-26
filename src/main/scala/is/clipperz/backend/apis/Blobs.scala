package is.clipperz.backend.apis

import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.Exceptions.*
import is.clipperz.backend.functions.{ fromStream }
import is.clipperz.backend.LogAspect
import is.clipperz.backend.services.{ BlobManager }

import zio.{ ZIO, Cause, Chunk }
import zio.http.{ Headers, Body, Method, FormField, Path, Response, Request, Routes, Status, handler }
import zio.http.codec.HeaderCodec
import zio.http.codec.PathCodec.string
import zio.http.Header.{ ContentType, ContentTransferEncoding }
import zio.stream.{ ZStream, ZSink }
import zio.nio.file.{ Files, Path as PathNIO }
import java.io.FileOutputStream
import java.security.MessageDigest
import zio.telemetry.opentelemetry.tracing.Tracing
import is.clipperz.backend.otel.PropagatorProvider
import is.clipperz.backend.otel.TracingAspect.EndpointTracer

private case class Identifier(value: HexString)
private case class Blob(identifier: Option[Identifier], hash: Option[HexString], data: Option[ZStream[Any, Nothing, Byte]])

val blobsApi: Routes[BlobManager & Tracing & PropagatorProvider, Throwable] = Routes(
    Method.POST / "api" / "blobs" -> handler: (request: Request) =>
        ZIO.scoped:
            ZIO.service[BlobManager]
            .zip(request.body.asMultipartFormStream)
            .flatMap((manager, streamingForm) => 
                streamingForm.fields
                .runFoldZIO(Blob(None, None, None))((result, field) => field match {
                    case FormField.StreamingBinary("identifier", contentType, transferEncoding, filename, data) =>
                        if result.identifier == None
                        then data.run(ZSink.collectAll[Byte])
                                 .map(_.toArray).map(bytes => Identifier(bytesToHex(bytes)))
                                 .map(identifier => Blob(Some(identifier), result.hash, result.data))
                        else ZIO.fail(new BadRequestException(s"Parameter 'identifier' specified more than once"))
                    case FormField.StreamingBinary("blob", contentType, transferEncoding, Some(filename), data) =>
                        if result.hash == None && result.data == None
                        then ZIO.succeed(Blob(result.identifier, Some(HexString(filename)), Some(data)))
                        else ZIO.fail(new BadRequestException(s"Parameter 'blob' specified more than once"))
                    case field =>
                        ZIO.fail(new BadRequestException(s"Invalid parameter '${field.name}'"))
                })
                .flatMap(blob => blob match {
                    case Blob(Some(Identifier(identifier)), Some(hash), Some(data)) =>
                        manager.saveBlob(hash, identifier, data)
                    case _ =>
                        ZIO.fail(new BadRequestException(s"Missing either/both 'blob', 'identifier' fields"))
                })
            )
            .map(result => Response.ok)
            @@ LogAspect.logAnnotateRequestData(request)
            @@ EndpointTracer()
,
    Method.DELETE / "api" / "blobs" / string("hash") -> handler: (hash: String, request: Request) =>
        ZIO
        .service[BlobManager]
        .zip(request.body.asMultipartFormStream)
        .flatMap((manager, streamingForm) => streamingForm
            .fields
            .collectZIO(field => field match {
                case FormField.StreamingBinary("identifier", contentType, transferEncoding,      filename,  data)   =>  data.run(ZSink.collectAll[Byte])
                                                                                                                            .map(_.toArray)
                                                                                                                            .flatMap(identifierBytes => manager.deleteBlob(HexString(hash), bytesToHex(identifierBytes)))
            })
            .runCount
        )
        .map:
            case 1  => Response.ok
            case _  => Response(status = Status.NotFound)
        @@ LogAspect.logAnnotateRequestData(request)
        @@ EndpointTracer()
,
    Method.GET / "api" / "blobs" / string("hash") -> handler: (hash: String, request: Request) =>
        ZIO
        .service[BlobManager]
        .flatMap(manager => manager.getBlob(HexString(hash)))
        .map((bytes: ZStream[Any, Throwable, Byte], contentLength: Long) =>
            Response(
                status = Status.Ok,
                body = Body.fromStream(bytes, contentLength),
                headers = Headers(ContentTransferEncoding.Binary)
                            .addHeader("Content-Type", "application/octet-stream"),
            )
        )
        @@ LogAspect.logAnnotateRequestData(request)
        @@ EndpointTracer()
)