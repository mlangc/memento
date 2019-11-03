package com.github.mlangc.memento

import org.scalactic.source.Position
import zio.Cause
import zio.FiberFailure
import zio.ZEnv
import zio.DefaultRuntime
import zio.ZIO

abstract class BaseZioTest extends BaseTest with DefaultRuntime {
  protected class FreeSpecZioStringWrapper(string: String, position: Position) {
    private val impl = new FreeSpecStringWrapper(string, position)

    def inIO[E, A](zio: ZIO[ZEnv, E, A]): Unit = {
      impl.in {
        unsafeRun(zio.either) match {
          case Right(_) => ()
          case Left(th: Throwable) => throw th
          case Left(other) => throw new FiberFailure(Cause.fail(other))
        }
      }
    }
  }

  protected implicit def convertToFreeSpecStringWrapperZio(s: String)(implicit pos: Position)
  : FreeSpecZioStringWrapper = new FreeSpecZioStringWrapper(s, pos)
}

