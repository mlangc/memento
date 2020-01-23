package com.github.mlangc.memento.trainer.examiner

import com.github.mlangc.memento.trainer.examiner.RepetitionHelpers.avoidRepeatRelated
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive
import zio.Ref
import zio.UIO
import zio.URIO
import zio.ZIO
import zio.random
import zio.random.Random
import zio.test.Assertion
import zio.test.DefaultRunnableSpec
import zio.test.TestAspect.nonFlaky
import zio.test.assert
import zio.test.suite
import zio.test.testM

object RepetitionHelpersTest extends DefaultRunnableSpec(
  suite("RepetitionHelpers")(
    suite("avoidRepeatRelated")(
      testM("behaves as expected when applied to random ints") {
        val nReps = 1000
        def repeat[R, E, A](zio: ZIO[R, E, A]) = ZIO.collectAll(Iterable.fill(nReps)(zio))

        def countRepetitions[A](as: List[A]) = as.grouped(2).count {
          case a1 :: a2 :: Nil if a1 == a2 => true
          case _ => false
        }

        for {
          cycle <- RepetitionHelperTestUtil.RandomCycle.make(2000, 5)
          randInts <- ZIO.foreach(0.to(3))(avoidRepeatRelated(cycle.iterator, _)(_ == _))
          ints <- ZIO.foreach(randInts)(r => repeat(r))
          reps = ints.map(countRepetitions)
        } yield assert(reps, Assertion.equalTo(reps.sortBy(-_)))
      } @@ nonFlaky(8),
      testM("properly deals with an effect that always returns 42") {
        for {
          io <- avoidRepeatRelated(ZIO.succeed(42))(_ == _)
          is <- ZIO.collectAll(Iterable.fill(42)(io))
        } yield assert(is, Assertion.equalTo(List.fill(42)(42)))
      }
    )
  )
)

object RepetitionHelperTestUtil {
  case class RandomCycle(start: Int, tail: IndexedSeq[Int]) {
    val iterator: UIO[UIO[Int]] = {
      Ref.make(0).map { ref =>
        ref.modify { i =>
          val v = if (i <= 0) start else tail(i - 1)
          val i2 = if (i == tail.length) 0 else i + 1

          (v, i2)
        }
      }
    }
  }

  object RandomCycle {
    def make(length: Int Refined Positive, modulo: Int Refined Positive): URIO[Random, RandomCycle] =
      for {
        start <- random.nextInt(modulo.value)
        tail <- ZIO.collectAll(Iterable.fill(length.value - 1)(random.nextInt(modulo.value))).map(_.toIndexedSeq)
      } yield RandomCycle(start, tail)
  }
}
