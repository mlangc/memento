package com.github.mlangc.memento.trainer.repetition.leitner

import java.time.Clock
import java.time.Duration
import java.time.Instant
import java.util.concurrent.ThreadLocalRandom

import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.instances.option._
import cats.syntax.traverse._
import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.repetition.RepetitionScheme
import com.github.mlangc.memento.trainer.repetition.RepetitionStatus
import com.github.mlangc.memento.trainer.repetition.RepetitionStatus.ShouldStop
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import scalaz.zio.Ref
import scalaz.zio.Task
import scalaz.zio.UIO
import scalaz.zio.interop.catz._

import scala.math.pow

class LeitnerRepetitionScheme(boxSpecs: NonEmptyVector[BoxSpec] = BoxSpecs.defaultBoxSpecs,
                              clock: Clock = Clock.systemDefaultZone()) extends RepetitionScheme {

  protected def implement(translations: NonEmptyVector[Translation],
                          checks: List[Check]): Task[RepetitionScheme.Impl] =
    for {
      now <- UIO(Instant.now(clock))
      deckStateRef <- Ref.make(initialDeckState(translations, checks, now))
    } yield {
      new RepetitionScheme.Impl {
        def next: Task[Question] =
          for {
            deckState <- deckStateRef.get
            question <- nextQuestion(deckState)
          } yield question

        def next(check: Check): Task[Question] =
          for {
            _ <- deckStateRef.update(_.updateWith(check).newState)
            question <- next
          } yield question

        def status: Task[RepetitionStatus] =
          for {
            now <- UIO(Instant.now(clock))
            deckState <- deckStateRef.update(_.recalculateAt(now).newState)
          } yield {
            val cardsLeft = deckState.cards.count(_._2._1.shouldBeTested)
            refineV[Positive](cardsLeft) match {
              case Right(cardsLeft) => RepetitionStatus.CardsLeft(cardsLeft)
              case Left(_) => ShouldStop
            }
          }
      }
    }

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
      case None => Task.fail(new IllegalStateException("Could not select a card"))
    }

    for {
      card <- selectCard
      question = Question(card.translation, card.direction)
    } yield question
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
      refineV[Positive](secondsRemaining).right.getOrElse(refineMV[Positive](1L))
    }.getOrElse(refineMV[Positive](1L))
  }

  private def selectRandomElemWeighted[A](as: Iterable[A])(weigh: A => Long Refined Positive): Task[Option[A]] = {
    val (totalWeight, asWithWeights) = as.foldLeft(0L -> List.empty[(A, Long Refined Positive)]) { case ((totalWeight, asWithWeights), a) =>
      val weight = weigh(a)
      (totalWeight + weight.value, (a -> weight) :: asWithWeights)
    }

    NonEmptyList.fromList(asWithWeights.sortBy(-_._2.value)).traverse { sortedAsWithWeights =>
      UIO(ThreadLocalRandom.current().nextLong(totalWeight)).flatMap { r =>
        UIO {
          selectForThreshold(
            sortedAsWithWeights,
            Refined.unsafeApply[Long, NonNegative](r))
        }
      }
    }
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

