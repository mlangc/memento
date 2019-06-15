package com.github.mlangc.memento.trainer.repetition.leitner

import java.time.ZoneId

import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.model.TestTrainingData
import com.github.mlangc.memento.trainer.repetition.GenericRepetitionSchemeTest
import com.github.mlangc.memento.trainer.repetition.RepetitionScheme
import com.github.mlangc.memento.trainer.repetition.RepetitionStatus
import com.github.mlangc.memento.trainer.repetition.leitner.TestBoxSpecs.defaultBoxSpecs
import com.statemachinesystems.mockclock.MockClock
import eu.timepit.refined.auto._
import scalaz.zio.Task

import scala.concurrent.duration._

class LeitnerRepetitionSchemeTest extends GenericRepetitionSchemeTest {

  protected lazy val scheme: RepetitionScheme = new LeitnerRepetitionScheme(defaultBoxSpecs)

  "Simulate with" - {
    "a single translation and no prior checks" inIO {
      val testData = TestTrainingData.enGerSingleElem
      val question1 = Question(testData.translations.head, Direction.LeftToRight)
      val question2 = question1.copy(direction = question1.direction.flip)

      for {
        clock <- Task(MockClock.at(2000, 1, 1, ZoneId.of("UTC")))
        scheme = new LeitnerRepetitionScheme(defaultBoxSpecs, clock)
        schemeImpl <- implFor(scheme, testData)
        firstStatus <- schemeImpl.status
        _ <- Task(assert(firstStatus === RepetitionStatus.CardsLeft(2)))
        firstReturnedQuestion <- schemeImpl.next(mkCheck(question2, Score.Perfect, clock))
        _ <- Task(firstReturnedQuestion === question1)
        _ <- schemeImpl.next(mkCheck(firstReturnedQuestion, Score.Perfect, clock))
        statusAfterFirstRound <- schemeImpl.status
        _ <- Task(assert(statusAfterFirstRound === RepetitionStatus.ShouldStop))

        _ <- advanceClock(clock, defaultBoxSpecs.getUnsafe(0).interval)
        statusBeforeSecondRound <- schemeImpl.status
        _ <- Task(assert(statusBeforeSecondRound === RepetitionStatus.CardsLeft(2)))
        _ <- schemeImpl.next(mkCheck(question1, Score.Perfect, clock))
        _ <- schemeImpl.next(mkCheck(question2, Score.Perfect, clock))
        statusAfterSecondRound <- schemeImpl.status
        _ <- Task(assert(statusAfterSecondRound === RepetitionStatus.ShouldStop))

        _ <- advanceClock(clock, defaultBoxSpecs.getUnsafe(1).interval)
        statusBeforeThirdRound <- schemeImpl.status
        _ <- Task(assert(statusBeforeThirdRound === RepetitionStatus.CardsLeft(2)))
        _ <- schemeImpl.next(mkCheck(question1, Score.Perfect, clock))
        _ <- schemeImpl.next(mkCheck(question2, Score.Perfect, clock))
        statusAfterThirdRound <- schemeImpl.status
        _ <- Task(assert(statusAfterThirdRound === RepetitionStatus.ShouldStop))

        _ <- advanceClock(clock, defaultBoxSpecs.getUnsafe(2).interval)
        statusBefore4thRound <- schemeImpl.status
        _ <- Task(assert(statusBefore4thRound === RepetitionStatus.CardsLeft(2)))
        _ <- schemeImpl.next(mkCheck(question1, Score.Perfect, clock))
        _ <- schemeImpl.next(mkCheck(question2, Score.Perfect, clock))
        statusAfter4thRound <- schemeImpl.status
        _ <- Task(assert(statusAfter4thRound === RepetitionStatus.ShouldStop))

        _ <- advanceClock(clock, defaultBoxSpecs.getUnsafe(3).interval)
        statusBefore5thRound <- schemeImpl.status
        _ <- Task(assert(statusBefore5thRound === RepetitionStatus.CardsLeft(2)))
        _ <- schemeImpl.next(mkCheck(question1, Score.Zero, clock))
        _ <- schemeImpl.next(mkCheck(question2, Score.SoSo, clock))
        statusAfter5thRound <- schemeImpl.status
        _ <- Task(assert(statusAfter5thRound === RepetitionStatus.CardsLeft(1)))

        _ <- advanceClock(clock, defaultBoxSpecs.getUnsafe(0).interval)
        statusBefore6thRound <- schemeImpl.status
        _ <- Task(assert(statusBefore6thRound === RepetitionStatus.CardsLeft(1)))

        _ <- advanceClock(clock, defaultBoxSpecs.getUnsafe(4).interval)
        statusBefore7thRound <- schemeImpl.status
        _ <- Task(assert(statusBefore7thRound === RepetitionStatus.CardsLeft(2)))
      } yield ()
    }
  }

  private def advanceClock(clock: MockClock, duration: Duration): Task[Unit] = Task {
    clock.advanceByMillis(duration.toMillis.toInt)
  }

  private def mkCheck(question: Question, score: Score, clock: MockClock): Check =
    Check(question.translation, question.direction, score, clock.instant())
}
