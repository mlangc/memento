package com.github.mlangc.memento.trainer.examiner

import com.github.mlangc.memento.trainer.examiner.RepetitionHelpers.avoidRepeatRelated
import zio.ZIO
import zio.random
import zio.test.Assertion
import zio.test.DefaultRunnableSpec
import zio.test.assert
import zio.test.suite
import zio.test.testM

object RepetitionHelpersTest extends DefaultRunnableSpec(
  suite("RepetitionHelpers")(
    suite("avoidRepeatRelated")(
      testM("behaves as expected when applied to random ints") {
        val nElems = 5
        val nReps = 1000
        val randInt = random.nextInt(nElems)
        def repeat[R, E, A](zio: ZIO[R, E, A]) = ZIO.collectAll(Iterable.fill(nReps)(zio))

        def countRepetitions[A](as: List[A]) = as.grouped(2).count {
          case a1 :: a2 :: Nil if a1 == a2 => true
          case _ => false
        }

        for {
          randInts <- ZIO.foreach(0.to(3))(avoidRepeatRelated(randInt, _)(_ == _))
          ints <- ZIO.foreach(randInts)(r => repeat(r))
          reps = ints.map(countRepetitions)
        } yield assert(reps, Assertion.equalTo(reps.sortBy(-_)))
      },
      testM("properly deals with an effect that always returns 42") {
        for {
          io <- avoidRepeatRelated(ZIO.succeed(42))(_ == _)
          is <- ZIO.collectAll(Iterable.fill(42)(io))
        } yield assert(is, Assertion.equalTo(List.fill(42)(42)))
      }
    )
  )
)
