package com.github.mlangc.memento

import org.scalactic.source.Position
import zio.{DefaultRuntime, ZIO}

abstract class BaseZioTest extends BaseTest with DefaultRuntime {
  protected class FreeSpecZioStringWrapper(string: String, position: Position) {
    private val impl = new FreeSpecStringWrapper(string, position)

    def inIO[E, A](zio: ZIO[Environment, E, A]): Unit = {
      impl.in(unsafeRun(zio))
    }
  }

  protected implicit def convertToFreeSpecStringWrapperZio(s: String)(implicit pos: Position)
  : FreeSpecZioStringWrapper = new FreeSpecZioStringWrapper(s, pos)
}

