package com.github.mlangc.memento.trainer.repetition.leitner

import java.time.Instant

import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import cats.instances.map._
import cats.instances.sortedSet._
import cats.syntax.option._
import cats.syntax.semigroup._
import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.trainer.model.Card

import scala.collection.View
import scala.collection.immutable.SortedSet
import scala.concurrent.duration.Duration
import scala.math.max
import scala.math.min

class DeckState private(val timestamp: Instant,
                        val boxSpecs: NonEmptyVector[BoxSpec],
                        private val cardChecks: Map[Card, SortedSet[Check]]) {
  def checks: List[Check] =
    cardChecks.values
      .flatMap(_.toList)
      .toSeq.sortBy(_.timestamp).toList

  lazy val cards: Map[Card, (CardState, BoxRef)] =
    cardChecks.view
      .map { case (card, checks) =>
        card -> processChecks(checks.view.takeWhile(!_.timestamp.isAfter(timestamp)))
      }.toMap

  def boxes: Map[BoxRef, Set[Card]] =
    cards
      .toSeq
      .map { case (card, (_, boxRef)) => (boxRef, card) }
      .groupMapReduce(_._1)(p => Set(p._2))(_ ++ _)

  def updateWith(check: Check): DeckStateUpdate =
    updateWith(Left(check))

  def recalculateAt(timestamp: Instant): DeckStateUpdate =
    updateWith(Right(timestamp))

  def updateWith(checkOrTimestamp: Either[Check, Instant]): DeckStateUpdate = {
    val newTimestamp = checkOrTimestamp match {
      case Left(check) if (check.timestamp.isAfter(timestamp)) => check.timestamp
      case Right(ts) => ts
      case _ => timestamp
    }

    val maybeCheck = checkOrTimestamp.left.toOption
    val newCardChecks = cardChecks |+| {
      maybeCheck.map { check =>
        if (!cardChecks.contains(Card.fromCheck(check))) Map.empty[Card, SortedSet[Check]]
        else Map(Card.fromCheck(check) -> SortedSet(check))
      }.getOrElse(Map.empty)
    }

    val newState = new DeckState(newTimestamp, boxSpecs, newCardChecks)
    val affectedCards =
      (newState.cards.keySet ++ cards.keySet).filter { card =>
        newState.cards.get(card) != cards.get(card)
      }

    DeckStateUpdate(newState, affectedCards)
  }

  private def processChecks(checks: View[Check]): (CardState, BoxRef) = {
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

  private def durationBetween(instant1: Instant, instant2: Instant): Duration = {
    val secDiff = instant2.getEpochSecond - instant1.getEpochSecond
    val nanoDiff = secDiff * 1000L * 1000L * 1000L + (instant2.getNano - instant1.getNano)
    Duration.fromNanos(nanoDiff)
  }
}

object DeckState {
  def init(translations: NonEmptyVector[Translation],
           boxSpecs: NonEmptyVector[BoxSpec],
           checks: NonEmptyList[Check]): DeckState = {
    val timestamp = checks.toList.maxBy(_.timestamp).timestamp
    val initialCards = cardChecksFrom(translations.toVector)

    new DeckState(timestamp, boxSpecs, cardChecksFrom(initialCards, checks.toList))
  }

  def init(translations: NonEmptyVector[Translation],
           boxSpecs: NonEmptyVector[BoxSpec],
           timestamp: Instant): DeckState = {
    new DeckState(timestamp, boxSpecs, cardChecksFrom(translations.toVector))
  }

  private def cardChecksFrom(translations: Iterable[Translation]): Map[Card, SortedSet[Check]] =
    translations.flatMap { translation =>
      val card = Card(translation, Direction.RightToLeft)
      List(card, card.flip)
    }.map(_ -> SortedSet.empty[Check])
      .toMap

  private def cardChecksFrom(initialCards: Map[Card, SortedSet[Check]], checks: Iterable[Check]): Map[Card, SortedSet[Check]] =
    initialCards |+| checks.filter(c => initialCards.contains(Card.fromCheck(c)))
      .groupBy(Card.fromCheck)
      .view.mapValues(vs => SortedSet.from(vs))
      .toMap
}
