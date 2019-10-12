package com.github.mlangc.memento.trainer.examiner

import cats.syntax.option._
import zio.Ref
import zio.UIO
import zio.ZIO

object RepetitionHelpers {
  def avoidRepeatRelated[R, E, A](choose: ZIO[R, E, A], maxTries: Int = 3)(related: (A, A) => Boolean): UIO[ZIO[R, E, A]] =
    if (maxTries < 1) ZIO.succeed(choose) else {
      def refine(last: Ref[Option[A]], tries: Int = 0): ZIO[R, E, A] =
        for {
          a <- choose
          a0 <- last.modify(l => (l, a.some))
          retry = tries <= maxTries && a0.exists(a0 => related(a0, a))
          a1 <- if (!retry) ZIO.succeed(a) else refine(last, tries + 1)
        } yield a1

      for {
        lastValue <- Ref.make(none[A])
        refined = refine(lastValue)
      } yield refined
    }
}
