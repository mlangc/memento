package com.github.mlangc.memento.util.convenience.syntax

import scalaz.zio.UIO

object either {
  implicit class EitherOps[L, R](val either: Either[L, R]) extends AnyVal {
    def orDie(msg: String): UIO[R] = either match {
      case Right(r) => UIO.succeed(r)
      case Left(_) => UIO.dieMessage(msg)
    }
  }
}
