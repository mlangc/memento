package com.github.mlangc.memento.trainer.repetition.leitner

import java.time.Instant
import java.time.ZoneId

import com.github.ghik.silencer.silent
import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.generators.TrainerGens
import com.github.mlangc.memento.trainer.model.TrainingData
import com.github.mlangc.memento.trainer.model.Card
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.model.TestTrainingData
import com.github.mlangc.memento.trainer.repetition.GenericRepetitionSchemeTest
import com.github.mlangc.memento.trainer.repetition.RepetitionStatus
import com.github.mlangc.memento.trainer.repetition.leitner.LeitnerRepetitionScheme.BoxInfoMotivator
import com.github.mlangc.memento.trainer.repetition.leitner.LeitnerRepetitionScheme.CardInfoMotivator
import com.github.mlangc.memento.trainer.repetition.leitner.TestBoxSpecs.defaultBoxSpecs
import com.github.mlangc.slf4zio.api._
import com.statemachinesystems.mockclock.MockClock
import eu.timepit.refined.auto._
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import zio.IO
import zio.Task
import zio.UIO

import scala.concurrent.duration._

@silent("non-Unit")
class LeitnerRepetitionSchemeTest extends GenericRepetitionSchemeTest with ScalaCheckPropertyChecks with LoggingSupport with OptionValues {

  protected lazy val scheme: LeitnerRepetitionScheme = new LeitnerRepetitionScheme(defaultBoxSpecs)

  "Simulate with" - {
    "a single translation and no prior checks" inIO {
      val testData = TestTrainingData.enGerSingleElem
      val question1 = Question(testData.translations.head, Direction.LeftToRight, 0)
      val question2 = question1.copy(direction = question1.direction.flip)

      for {
        clock <- Task(MockClock.at(2000, 1, 1, ZoneId.of("UTC")))
        scheme = new LeitnerRepetitionScheme(defaultBoxSpecs, clock)
        schemeImpl <- implFor(scheme, testData)
        firstStatus <- schemeImpl.status
        _ <- Task(assert(firstStatus === RepetitionStatus.CardsLeft(2)))
        firstReturnedQuestion <- schemeImpl.next(mkCheck(question2, Score.Perfect, clock))
        _ <- Task(assert(firstReturnedQuestion.toCard === question1.toCard))
        _ <- checkMotivators(firstReturnedQuestion) { (cardInfo, boxInfo) =>
          assert(cardInfo.cardState === CardState.New)
          assert(boxInfo.currentBox.index === 0)
          assert(boxInfo.currentBox.spec === defaultBoxSpecs.getUnsafe(0))
          assert(boxInfo.nextBox.value === defaultBoxSpecs.getUnsafe(1))
          assert(boxInfo.canAdvance === false)
        }
        _ <- schemeImpl.next(mkCheck(firstReturnedQuestion, Score.Perfect, clock))
        statusAfterFirstRound <- schemeImpl.status
        _ <- Task(assert(statusAfterFirstRound === RepetitionStatus.ShouldStop))

        _ <- advanceClock(clock, defaultBoxSpecs.getUnsafe(0).interval)
        statusBeforeSecondRound <- schemeImpl.status
        _ <- Task(assert(statusBeforeSecondRound === RepetitionStatus.CardsLeft(2)))
        _ <- schemeImpl.next(mkCheck(question1, Score.Perfect, clock)).flatMap { question =>
          checkMotivators(question) { (cardInfo, boxInfo) =>
            assert(cardInfo.cardState === CardState.Expired)
            assert(boxInfo.canAdvance === true)
            assert(boxInfo.currentBox.index === 0)
            assert(boxInfo.nextBox.value === defaultBoxSpecs.getUnsafe(1))
          }
        }
        _ <- schemeImpl.next(mkCheck(question2, Score.Perfect, clock))
        statusAfterSecondRound <- schemeImpl.status
        _ <- Task(assert(statusAfterSecondRound === RepetitionStatus.ShouldStop))

        _ <- advanceClock(clock, defaultBoxSpecs.getUnsafe(1).interval)
        statusBeforeThirdRound <- schemeImpl.status
        _ <- Task(assert(statusBeforeThirdRound === RepetitionStatus.CardsLeft(2)))
        _ <- schemeImpl.next(mkCheck(question1, Score.Perfect, clock)).flatMap { question =>
          checkMotivators(question) { (cardInfo, boxInfo) =>
            assert(cardInfo.cardState === CardState.Expired)
            assert(boxInfo.currentBox.index === 1)
            assert(boxInfo.canAdvance === true)
            assert(boxInfo.nextBox.value === defaultBoxSpecs.getUnsafe(2))
          }
        }
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

    "generated data" - {
      lazy val trainerGens = new TrainerGens()

      def mkSchemeImplWithMockClock(trainingData: TrainingData): Task[Option[(LeitnerRepetitionScheme.Impl, MockClock)]] = {
        val latestCheckTs = {
          if (trainingData.checks.isEmpty) Instant.EPOCH
          else trainingData.checks.maxBy(_.timestamp).timestamp
        }

        mkSchemeWithMockClock(latestCheckTs).flatMap { case (scheme, clock) =>
          scheme.implement(trainingData).map {
            case Some(impl: LeitnerRepetitionScheme.Impl) => Some(impl -> clock)
            case _ => None
          }
        }
      }

      lazy val schemeImplWithMockClockGen: Gen[Task[Option[(LeitnerRepetitionScheme.Impl, MockClock)]]] =
        trainerGens.trainingData
          .map(data => mkSchemeImplWithMockClock(data))

      "making sure that dormant questions are not asked unless needed" in {
        forAll(schemeImplWithMockClockGen) { schemeImplWithMockClock =>
          unsafeRun {
            schemeImplWithMockClock.flatMap {
              case None => IO.unit
              case Some((impl, clock)) =>

                def questionsTillFirstOffenderOrShouldStop(acc: List[Question] = Nil): Task[List[Question]] = {
                  def isOffender(question: Question, deckState: DeckState): Boolean = {
                    val cardState = deckState.cards(Card(question.translation, question.direction))._1
                    !cardState.shouldBeTested
                  }

                  for {
                    _ <- UIO(clock.advanceBySeconds(15))
                    now <- UIO(clock.instant())
                    status <- impl.status
                    deckState <- impl.getDeckState
                    questions <- {
                      if (status.shouldStop) IO.succeed(acc) else {
                        val question = acc match {
                          case Nil => impl.next
                          case lastQuestion :: _ => impl.next(Check(
                            lastQuestion.translation, lastQuestion.direction, Score.Perfect, now))
                        }

                        question.flatMap { question =>
                          if (isOffender(question, deckState)) Task.succeed(question :: acc)
                          else questionsTillFirstOffenderOrShouldStop(question :: acc)
                        }
                      }
                    }
                  } yield questions
                }

                for {
                  questions <- questionsTillFirstOffenderOrShouldStop()
                  shouldStop <- impl.status.map(_.shouldStop)
                  _ <- Task(assert(shouldStop, questions))
                } yield ()
            }
          }
        }
      }

      "make sure that signals that `shouldStop` as soon as all questions are dormant" in {
        forAll(schemeImplWithMockClockGen) { implWithMockGen =>
          unsafeRun {
            implWithMockGen.flatMap {
              case None => IO.unit
              case Some((impl, clock)) =>
                def tryFindOffendingSequence(acc: List[Check] = Nil): Task[Option[List[Check]]] = {
                  val getNextQuestion: Task[Question] = acc match {
                    case Nil => impl.next
                    case check :: _ => impl.next(check)
                  }

                  for {
                    question <- getNextQuestion
                    cardState <- getCardStateFromMotivators(question)
                    schemeState <- impl.status
                    foundOffender = !schemeState.shouldStop && !cardState.shouldBeTested
                    shouldGiveUp = !foundOffender && schemeState.shouldStop
                    res <- {
                      if (foundOffender) {
                        Task.succeed(Some(acc))
                      } else if (shouldGiveUp) {
                        Task.succeed(None)
                      } else {
                        advanceClock(clock, 15.seconds) *> {
                          val check = mkCheck(question, Score.Perfect, clock)
                          tryFindOffendingSequence(check :: acc)
                        }
                      }
                    }
                  } yield res
                }

                for {
                  offendingSeq <- tryFindOffendingSequence()
                  _ <- offendingSeq match {
                    case None => Task.unit
                    case Some(offender) => Task.fail(new AssertionError(s"Found an offending sequence: $offender"))
                  }
                } yield ()
            }
          }
        }
      }
    }
  }

  private def getCardStateFromMotivators(question: Question): Task[CardState] = Task {
    question.motivators.collectFirst {
      case CardInfoMotivator(cardState) => cardState
    }.value
  }

  private def mkSchemeWithMockClock(now: Instant): Task[(LeitnerRepetitionScheme, MockClock)] =
    for {
      clock <- Task(MockClock.at(now, ZoneId.of("UTC")))
      scheme = new LeitnerRepetitionScheme(defaultBoxSpecs, clock)
    } yield (scheme, clock)

  private def advanceClock(clock: MockClock, duration: Duration): Task[Unit] = Task {
    clock.advanceByMillis(duration.toMillis.toInt)
  }

  private def mkCheck(question: Question, score: Score, clock: MockClock): Check =
    Check(question.translation, question.direction, score, clock.instant())

  private def checkMotivators(question: Question)(checks: (CardInfoMotivator, BoxInfoMotivator) => Unit): Task[Unit] = Task {
    question.motivators match {
      case (boxInfo: BoxInfoMotivator) :: (cardInfo: CardInfoMotivator) :: Nil => checks(cardInfo, boxInfo)
      case (cardInfo: CardInfoMotivator) :: (boxInfo: BoxInfoMotivator) :: Nil => checks(cardInfo, boxInfo)
      case other => throw new AssertionError("" + other)
    }
  }
}
