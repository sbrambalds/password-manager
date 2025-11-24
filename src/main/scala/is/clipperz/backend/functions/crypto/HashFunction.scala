package is.clipperz.backend.functions.crypto

import java.security.MessageDigest
import zio.{ Task, Chunk }
import zio.stream.{ ZStream, ZSink }

type HashFunction = ZStream[Any, Throwable, Byte] => Task[Array[Byte]]

object HashFunction:
  val hashSHA256: HashFunction = (bytes: ZStream[Any, Throwable, Byte]) =>
    val digester: MessageDigest = MessageDigest.getInstance("SHA-256").nn
    bytes
      .run(ZSink.digest(digester))
      .map((chunk: Chunk[Byte]) => chunk.toArray)

  val hashSHA1: HashFunction = (bytes: ZStream[Any, Throwable, Byte]) =>
    val digester: MessageDigest = MessageDigest.getInstance("SHA-1").nn
    bytes
      .run(ZSink.digest(digester))
      .map((chunk: Chunk[Byte]) => chunk.toArray)
