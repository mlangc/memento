package com.github.mlangc.memento.db.cache

import java.nio.ByteBuffer
import java.nio.charset.CharacterCodingException
import java.nio.charset.CharsetDecoder
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets

import cats.syntax.either._
import com.github.mlangc.memento.db.model.Check

trait BasicCachables {
  implicit val stringCachable: Cachable[String] = new Cachable[String] {
    private val decoders = new ThreadLocal[CharsetDecoder] {
      override protected def initialValue(): CharsetDecoder =
        StandardCharsets.UTF_8.newDecoder()
          .onMalformedInput(CodingErrorAction.REPORT)
          .onUnmappableCharacter(CodingErrorAction.REPORT)
    }

    def toBytes(a: String): Array[Byte] = a.getBytes(StandardCharsets.UTF_8)

    def fromBytes(bytes: Array[Byte]): Either[IllegalArgumentException, String] =
      try {
        decoders.get().decode(ByteBuffer.wrap(bytes)).toString.asRight
      } catch {
        case e: CharacterCodingException =>
          new IllegalArgumentException("Expected an UTF-8 byte sequence", e).asLeft
      }
  }

  implicit val shortCachable: Cachable[Short] = new Cachable[Short] {
    def toBytes(a: Short): Array[Byte] = {
      val res = new Array[Byte](2)
      res(0) = (a & 0xff).toByte
      res(1) = ((a >> 8) & 0xff).toByte
      res
    }

    def fromBytes(bytes: Array[Byte]): Either[IllegalArgumentException, Short] =
      if (bytes.length != 2) new IllegalArgumentException(s"Cannot construct short from ${bytes.length} bytes").asLeft
      else ((bytes(0) & 0xff) | ((bytes(1) & 0xff) << 8)).toShort.asRight
  }

  implicit val intCachable: Cachable[Int] = new Cachable[Int] {
    def toBytes(a: Int): Array[Byte] = {
      val buffer = ByteBuffer.allocate(4)
      buffer.putInt(a)
      buffer.array()
    }

    def fromBytes(bytes: Array[Byte]): Either[IllegalArgumentException, Int] =
      if (bytes.length != 4) new IllegalArgumentException(s"Cannot construct int from ${bytes.length} bytes").asLeft
      else ByteBuffer.wrap(bytes).getInt(0).asRight
  }

  implicit val checksCachable: Cachable[Iterable[Check]] = new Cachable[Iterable[Check]] {
    import io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._, io.circe.refined._

    def toBytes(checks: Iterable[Check]): Array[Byte] =
      checks.asJson.spaces2.getBytes(StandardCharsets.UTF_8)

    def fromBytes(bytes: Array[Byte]): Either[IllegalArgumentException, Iterable[Check]] =
      for {
        json <- parse(new String(bytes, StandardCharsets.UTF_8)).leftMap(new IllegalArgumentException("Parse error", _))
        checks <- json.as[Iterable[Check]].leftMap(new IllegalArgumentException("Decoding error", _))
      } yield checks
  }
}

object BasicCachables extends BasicCachables
