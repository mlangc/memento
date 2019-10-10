package com.github.mlangc.memento.trainer.repetition.leitner

import java.time.Instant

import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.instances.list._
import cats.instances.map._
import cats.kernel.Order
import cats.syntax.option._
import cats.syntax.semigroup._
import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.trainer.model.Card

import scala.concurrent.duration.Duration
import scala.math.max
import scala.math.min

class DeckState private(val timestamp: Instant,
                        val boxSpecs: NonEmptyVector[BoxSpec],
                        val checks: List[Check],
                        val cards: Map[Card, (CardState, BoxRef)],
                        val boxes: Map[BoxRef, Set[Card]]) {

  def updateWith(check: Check): DeckStateUpdate =
    updateWith(Left(check))

  def recalculateAt(timestamp: Instant): DeckStateUpdate =
    updateWith(Right(timestamp))

  private def updateWith(checkOrTimestamp: Either[Check, Instant]): DeckStateUpdate = {
    val newTimestamp = checkOrTimestamp match {
      case Left(check) if (check.timestamp.isAfter(timestamp)) => check.timestamp
      case Right(ts) => ts
      case _ => timestamp
    }

    val maybeCheck = checkOrTimestamp.left.toOption
    val effectiveChecks = maybeCheck.toList ::: checks.filterNot(_.timestamp.isAfter(newTimestamp))
    val newCards = DeckState.processCards(cards.keySet, newTimestamp, effectiveChecks, boxSpecs)
    val newBoxes = DeckState.boxesFor(newCards)

    val changedCards = cards.keySet.filter { card =>
      cards(card) != newCards(card)
    }

    val newState = new DeckState(newTimestamp, boxSpecs, maybeCheck.toList ::: checks, newCards, newBoxes)
    DeckStateUpdate(newState, changedCards)
  }

  override def toString: String = {
    s"DeckState ${(timestamp, boxSpecs, cards)}"
  }

}

object DeckState {
  def init(translations: NonEmptyVector[Translation],
           boxSpecs: NonEmptyVector[BoxSpec],
           checks: NonEmptyList[Check]): DeckState = {
    val checksOrdered = checks.sortBy(_.timestamp)(Order.fromComparable[Instant])
    val initialState = init(translations, boxSpecs, checksOrdered.head.timestamp)
    checksOrdered.foldLeft(initialState) { (state, check) =>
      state.updateWith(check).newState
    }
  }

  def init(translations: NonEmptyVector[Translation],
           boxSpecs: NonEmptyVector[BoxSpec],
           timestamp: Instant): DeckState = {
    val cards = newCardsFor(translations.toVector, boxSpecs.head)
    new DeckState(timestamp, boxSpecs, Nil, cards, boxesFor(cards))
  }

  private def processCards(cards: Set[Card],
                           timestamp: Instant,
                           checks: List[Check],
                           boxSpecs: NonEmptyVector[BoxSpec])
  : Map[Card, (CardState, BoxRef)] = {
    def processChecksForCard(checks: List[Check]): (CardState, BoxRef) = {
      val ((tmpCardState, boxSpecInd), lastTs) = checks
        .foldLeft((CardState.New: CardState, 0) -> none[Instant]) { case (((oldState, boxInd), oldTimestamp), check) =>
          val boxSpec = boxSpecs.getUnsafe(boxInd)
          val timePassed = oldTimestamp.map(durationBetween(_, check.timestamp)).getOrElse(Duration.Zero)
          val baseState = oldState match {
            case CardState.Dormant if (timePassed >= boxSpec.interval) => CardState.Expired
            case other => other
          }

          val baseStep = {
            val diff = check.score.ordinal - boxSpec.minScore.ordinal
            if (diff >= 0) diff + 1 else diff
          }

          val refinedBaseStep = baseState match {
            case CardState.Dormant | CardState.Downgraded | CardState.New => min(0, baseStep)
            case CardState.Expired => min(1, baseStep)
          }

          val newBoxInd = min(max(boxInd + refinedBaseStep, 0), boxSpecs.length - 1)
          val newBoxSpec = boxSpecs.getUnsafe(newBoxInd)

          val newState = if (baseStep < 0) CardState.Downgraded else {
            if (newBoxSpec.interval > Duration.Zero) CardState.Dormant
            else CardState.Expired
          }

          (newState, newBoxInd) -> Some(check.timestamp)
      }

      val boxRef = BoxRef(boxSpecs.getUnsafe(boxSpecInd), boxSpecInd)
      (lastTs, tmpCardState) match {
        case (Some(lastTs), CardState.Dormant) if lastTs != timestamp =>
          val timePassed = durationBetween(lastTs, timestamp)
          if (timePassed >= boxRef.spec.interval) (CardState.Expired, boxRef)
          else (tmpCardState, boxRef)

        case _ => (tmpCardState, boxRef)
      }
    }

    val checksForCard: Map[Card, List[Check]] = checks
      .groupBy(check => Card(check.translation, check.direction)) |+| cards.map(_ -> List.empty[Check]).toMap

    checksForCard
      .view.filterKeys(cards.contains)
      .toMap
      .map { case (card, checks) => card -> processChecksForCard(normalizedAndSorted(checks)) }
  }

  private def normalizedAndSorted(checks: List[Check]): List[Check] = {
    checks.distinct.sortBy(c => (c.timestamp, c.score.ordinal))
  }

  private def newCardsFor(translations: Iterable[Translation], boxSpec: BoxSpec): Map[Card, (CardState, BoxRef)] = {
    val newInBox0 = CardState.New -> BoxRef(boxSpec, 0)

    translations.flatMap { translation =>
      Iterable(Card(translation, Direction.RightToLeft), Card(translation, Direction.LeftToRight))
    }.map(card => card -> newInBox0).toMap
  }

  private def durationBetween(instant1: Instant, instant2: Instant): Duration = {
    val secDiff = instant2.getEpochSecond - instant1.getEpochSecond
    val nanoDiff = secDiff * 1000L * 1000L * 1000L + (instant2.getNano - instant1.getNano)
    Duration.fromNanos(nanoDiff)
  }

  private def boxesFor(cards: Map[Card, (CardState, BoxRef)]): Map[BoxRef, Set[Card]] = {
    cards.toSeq
      .map(entry => entry._2._2 -> entry._1)
      .groupBy(_._1)
      .view.mapValues(_.map(_._2).toSet)
      .toMap
  }
}
