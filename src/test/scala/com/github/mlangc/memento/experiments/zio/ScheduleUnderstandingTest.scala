package com.github.mlangc.memento.experiments.zio

import com.github.mlangc.memento.BaseZioTest
import com.github.mlangc.slf4zio.api._
import org.scalatest.EitherValues
import zio.IO
import zio.Ref
import zio.Schedule
import zio.Task
import zio.UIO

class ScheduleUnderstandingTest extends BaseZioTest with LoggingSupport with EitherValues {
  "repeating" - {
    "n-times" inIO {
      val n = 3
      val schedule = Schedule.identity[Int] <* Schedule.recurs(3)
      for {
        effect <- mkGetAndDecrement
        res <- effect.repeat(schedule)
        _ <- Task(assert(res === -n))
      } yield ()
    }
  }

  "retrying" - {
    "n-times" inIO {
      val schedule: Schedule[Any, Int] = Schedule.recurs(2)
      for {
        effect <- mkFailUntilRetries(5)
        res1 <- effect.retry(schedule).either
        _ <- Task(assert(res1.left.value.toString.startsWith("Retry")))
        res2 <- effect.retry(schedule)
        _ <- Task(assert(res2 === -5))
      } yield ()
    }
  }

  private def mkGetAndDecrement: UIO[UIO[Int]] =
    Ref.make(0).map { ref =>
      (ref.get <* ref.update(_ - 1)).tap { ret =>
        logger.debugIO(s"mkGetAndDecrement -> $ret")
      }
    }

  private def mkFailUntilRetries(n: Int): UIO[IO[String, Int]] =
    mkGetAndDecrement.map { effect =>
      effect.flatMap { i =>
        val retriesMissing = n + i
        if (retriesMissing > 0) IO.fail(s"Retry $retriesMissing more times")
        else IO.succeed(i)
      }
  }
}
