package com.github.mlangc.memento

import org.scalactic.source.Position
import org.scalatest.exceptions.TestCanceledException
import zio.Exit
import zio.FiberFailure
import zio.Task
import zio.ZEnv
import zio.ZIO
import zio.Runtime

abstract class BaseZioTest extends BaseTest {
  protected class FreeSpecZioStringWrapper(string: String, position: Position) {
    private val impl = new FreeSpecStringWrapper(string, position)

    def inIO[E, A](zio: ZIO[ZEnv, E, A]): Unit = {
      impl.in {
        Runtime.default.unsafeRun(zio.run.provideLayer(ZEnv.live)) match {
          case Exit.Success(_) => ()
          case Exit.Failure(cause) =>
            cause.failureOption match {
              case Some(e: TestCanceledException) => throw e
              case _ => throw FiberFailure(cause)
            }
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

