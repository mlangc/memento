package com.github.mlangc.memento.trainer.repetition.leitner

import java.time.Clock
import java.time.Duration
import java.time.Instant
import java.util.concurrent.ThreadLocalRandom

import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.i18n.MotivatorMessages
import com.github.mlangc.memento.trainer.model.Question.Motivator
import com.github.mlangc.memento.trainer.model.Card
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.repetition.RepetitionStatus.ShouldStop
import com.github.mlangc.memento.trainer.repetition.leitner.LeitnerRepetitionScheme.BoxInfoMotivator
import com.github.mlangc.memento.trainer.repetition.leitner.LeitnerRepetitionScheme.CardInfoMotivator
import com.github.mlangc.memento.trainer.repetition.RepetitionScheme
import com.github.mlangc.memento.trainer.repetition.RepetitionStatus
import com.github.mlangc.slf4zio.api._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import eu.timepit.refined._
import zio.Ref
import zio.Task
import zio.UIO

import scala.math.pow

class LeitnerRepetitionScheme(boxSpecs: NonEmptyVector[BoxSpec] = BoxSpecs.defaultBoxSpecs,
                              clock: Clock = Clock.systemDefaultZone()) extends RepetitionScheme with LoggingSupport {

  protected def implement(translations: NonEmptyVector[Translation],
                          checks: List[Check]): Task[LeitnerRepetitionScheme.Impl] = {
    for {
      now <- UIO(Instant.now(clock))
      deckStateRef <- Ref.make(initialDeckState(translations, checks, now))
    } yield {
      new LeitnerRepetitionScheme.Impl {
        val next: Task[Question] =
          for {
            deckState <- deckStateRef.get
            question <- nextQuestion(deckState)
          } yield question

        def update(check: Check): Task[Unit] =
          deckStateRef.update(_.updateWith(check).newState).unit

        val status: Task[RepetitionStatus] =
          for {
            now <- UIO(Instant.now(clock))
            deckState <- deckStateRef.updateAndGet(_.recalculateAt(now).newState)
            status = {
              val cardsLeft = deckState.cards.count(_._2._1.shouldBeTested)
              refineV[Positive](cardsLeft) match {
                case Right(cardsLeft) => RepetitionStatus.CardsLeft(cardsLeft)
                case Left(_) => ShouldStop
              }
            }
          } yield status

        val getDeckState: UIO[DeckState] = deckStateRef.get
      }
    }
  }.perfLog(LogSpec.onSucceed(d => debug"${d.render} for implementing ${getClass.getSimpleName}"))

  private def initialDeckState(translations: NonEmptyVector[Translation],
                               checks: List[Check],
                               now: Instant): DeckState = {
    NonEmptyList.fromList(checks)
      .map(DeckState.init(translations, boxSpecs, _))
      .getOrElse(DeckState.init(translations, boxSpecs, now))
  }

  private def nextQuestion(deckState: DeckState): Task[Question] = {
    val cardsToBeTested = {
      deckState.cards.collect { case (card, (state, _)) if state.shouldBeTested => card }
    }

    val selectCard: Task[Card] = selectRandomElemWeighted(cardsToBeTested)(weighCardsByBoxRef(deckState)).flatMap {
      case Some(card) => UIO.succeed(Some(card))
      case _ =>
        val remainingCards = deckState.cards.keySet.diff(cardsToBeTested.toSet)
        for {
          now <- UIO(Instant.now(clock))
          card <- selectRandomElemWeighted(remainingCards)(weighCardBySecondsTillExpired(now, deckState))
        } yield card
    }.flatMap {
      case Some(card) => UIO.succeed(card)
      case None => Task.die(new IllegalStateException("Could not select a card"))
    }

    for {
      card <- selectCard
      question = Question.create(card.translation, card.direction, deckState.checks, motivators = motivatorsFor(deckState, card))
    } yield question
  }

  private def motivatorsFor(deckState: DeckState, card: Card): List[Motivator] = {
    boxInfoMotivatorFor(deckState, card) :: cardInfoMotivator(deckState, card) :: Nil
  }

  private def boxInfoMotivatorFor(deckState: DeckState, card: Card): BoxInfoMotivator = {
    val (cardState, boxRef) = deckState.cards(card)
    val nextBox = deckState.boxSpecs.get(boxRef.index + 1)
    val canAdvance = cardState == CardState.Expired
    BoxInfoMotivator(boxRef, nextBox, canAdvance)
  }

  private def cardInfoMotivator(deckState: DeckState, card: Card): CardInfoMotivator = {
    CardInfoMotivator(deckState.cards(card)._1)
  }

  private def weighCardsByBoxRef(deckState: DeckState)(card: Card): Long Refined Positive = {
    val factor = 1.25
    val baseWeight = 100
    val boxInd = deckState.cards(card)._2.index

    Refined.unsafeApply[Long, Positive] {
      (pow(factor, boxInd.toDouble) * baseWeight).toLong
    }
  }

  private def weighCardBySecondsTillExpired(now: Instant, deckState: DeckState)(card: Card): Long Refined Positive = {
    val lastCheck = deckState.checks
      .filter(check => card.correspondsTo(check) && check.timestamp.isBefore(now))
      .sortBy(_.timestamp)
      .lastOption

    lastCheck.map { lastCheck =>
      val secondsSinceLastCheck = Duration.between(lastCheck.timestamp, now).getSeconds
      val secondsTillExpired = deckState.cards(card)._2.spec.interval.toSeconds
      val secondsRemaining = secondsTillExpired - secondsSinceLastCheck
      refineV[Positive](secondsRemaining).getOrElse(refineMV[Positive](1L))
    }.getOrElse(refineMV[Positive](1L))
  }

  private def selectRandomElemWeighted[A](as: Iterable[A])(weigh: A => Long Refined Positive): Task[Option[A]] = {
    val (totalWeight, asWithWeights) = as.foldLeft(0L -> List.empty[(A, Long Refined Positive)]) { case ((totalWeight, asWithWeights), a) =>
      val weight = weigh(a)
      (totalWeight + weight.value, (a -> weight) :: asWithWeights)
    }

    NonEmptyList.fromList(asWithWeights.sortBy(-_._2.value)).map { sortedAsWithWeights =>
      UIO(ThreadLocalRandom.current().nextLong(totalWeight)).flatMap { r =>
        UIO {
          Some {
            selectForThreshold(
              sortedAsWithWeights,
              Refined.unsafeApply[Long, NonNegative](r))
          }
        }
      }
    }.getOrElse(UIO.succeed(None))
  }

  @scala.annotation.tailrec
  private def selectForThreshold[A](asWithWeights: NonEmptyList[(A, Long Refined Positive)], threshold: Long Refined NonNegative): A = {
    asWithWeights match {
      case NonEmptyList(aw, Nil) => aw._1
      case NonEmptyList((a, weight), t1 :: ts) =>
        refineV[NonNegative](threshold.value - weight.value) match {
          case Right(newThreshold) => selectForThreshold(NonEmptyList(t1, ts), newThreshold)
          case _ => a
        }
    }
  }
}

object LeitnerRepetitionScheme {

  trait Impl extends RepetitionScheme.Impl {
    private[leitner] def getDeckState: UIO[DeckState]
  }

  case class BoxInfoMotivator(currentBox: BoxRef, nextBox: Option[BoxSpec], canAdvance: Boolean) extends Motivator {
    def text(messages: MotivatorMessages): String = {
      nextBox.map { nextBox =>
        messages.boxInfo(currentBox.index, currentBox.spec.minScore, currentBox.spec.interval, nextBox.minScore, nextBox.interval, canAdvance)
      }.getOrElse(messages.boxInfo(currentBox.index, currentBox.spec.minScore, currentBox.spec.interval))
    }
  }

  case class CardInfoMotivator(cardState: CardState) extends Motivator {
    def text(messages: MotivatorMessages) = messages.cardState(cardState)
  }

}

