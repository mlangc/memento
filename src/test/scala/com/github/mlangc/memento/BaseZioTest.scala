package com.github.mlangc.memento

import org.scalactic.source.Position
import org.scalatest.exceptions.TestCanceledException
import zio.Cause
import zio.FiberFailure
import zio.ZEnv
import zio.DefaultRuntime
import zio.Task
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

  protected implicit class ZioScalatestOps[R, E, A](zio: ZIO[R, E, A]) {
    def orCancelTest: ZIO[R, TestCanceledException, A] = {
      zio.catchAll {
        case e: Throwable => Task(cancel(e)).refineToOrDie[TestCanceledException]
        case o => Task(cancel("" + o)).refineToOrDie[TestCanceledException]
      }
    }
  }
}

