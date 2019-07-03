package com.github.mlangc.memento.trainer.repetition.leitner

import java.time.Instant.EPOCH

import cats.data.NonEmptyVector
import com.github.ghik.silencer.silent
import com.github.mlangc.memento.BaseTest
import com.github.mlangc.memento.db.model.Direction._
import com.github.mlangc.memento.db.model.Score.{Good, Perfect, SoSo}
import com.github.mlangc.memento.db.model.{Check, Score, Translation}
import com.github.mlangc.memento.generators.LeitnerGens
import com.github.mlangc.memento.trainer.model.Card
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.concurrent.duration._
import scala.util.Random
import eu.timepit.refined.auto._

@silent("non-Unit")
class DeckStateTest extends BaseTest with ScalaCheckDrivenPropertyChecks {
  private val leitnerGens = new LeitnerGens()
  private def baseGens = leitnerGens.baseGens

  "Look at a deck with a single translation" - {
    "and a single box" in {
      val theBoxSpec = BoxSpec(1.day, Good)
      val theBoxRef = BoxRef(theBoxSpec, 0)
      val theTranslation = Translation("hell", "Hölle")
      val state0 = DeckState.init(NonEmptyVector.of(theTranslation), NonEmptyVector.of(theBoxSpec), EPOCH)

      assert(state0.timestamp === EPOCH)
      assert(state0.boxSpecs.head === theBoxSpec)
      assert(state0.boxes.size === 1)
      assert(state0.checks.isEmpty)
      assert(state0.cards === Map(
        Card(theTranslation, LeftToRight) -> (CardState.New -> theBoxRef),
        Card(theTranslation, RightToLeft) -> (CardState.New -> theBoxRef)))

      val update1 = state0.updateWith(Check(theTranslation, LeftToRight, Perfect, EPOCH.plusSeconds(1)))
      assert(update1.affectedCards === Set(Card(theTranslation, LeftToRight)))

      val state1 = update1.newState
      assert(state1.checks.size === 1)
      assert(state1.cards.count(_._2._1 == CardState.Dormant) === 1)
      assert(state1.cards.count(_._2._1 == CardState.New) === 1)

      val update2 = state1.updateWith(Check(theTranslation, RightToLeft, SoSo, EPOCH.plusSeconds(60)))
      assert(update2.affectedCards.size === 1)

      val state2 = update2.newState
      assert(state2.checks.size === 2)
      assert(state2.cards.count(_._2._1 == CardState.Dormant) === 1)
      assert(state2.cards.count(_._2._1 == CardState.Downgraded) === 1)

      val update3 = state2.updateWith(Check(theTranslation, RightToLeft, Good, EPOCH.plusSeconds(180)))
      assert(update3.affectedCards.size === 1)

      val state3 = update3.newState
      assert(state3.checks.size === 3)
      assert(state3.cards.forall(_._2._1 == CardState.Dormant))

      val twoDaysAfterEpoch = EPOCH.plusSeconds(3600 * 24 * 2)
      val update4 = state3.recalculateAt(twoDaysAfterEpoch)
      assert(update4.affectedCards.size === 2)

      val state4 = update4.newState
      assert(state4.timestamp === twoDaysAfterEpoch)
      assert(state4.checks === state3.checks)
      assert(state4.cards.forall(_._2._1 == CardState.Expired))

      val update5 = state4.updateWith(Check(theTranslation, LeftToRight, Good, twoDaysAfterEpoch))
      assert(update5.affectedCards.size === 1)

      val state5 = update5.newState
      assert(state5.cards.count(_._2._1 == CardState.Dormant) === 1)
      assert(state5.cards.count(_._2._1 == CardState.Expired) === 1)

      val update6 = state5.recalculateAt(EPOCH)
      assert(update6.affectedCards.size === 2)

      val state6 = update6.newState
      assert(state6.boxes === state0.boxes)
      assert(state6.cards === state0.cards)
    }
  }

  "DeckState properties" - {
    "changed cards are changed indeed, timestamps are properly set, checks are added, and the detected changes make sense" in {
      forAll(leitnerGens.deckStateWithChecks()) { case (initialState, checks) =>
        assertSane(initialState)
        assertTimestampAfterAllChecks(initialState)

        val latestCheck = if (initialState.checks.isEmpty) initialState.timestamp else {
          initialState.checks.maxBy(_.timestamp).timestamp
        }

        assert(latestCheck === initialState.timestamp)
        val finalState = updateWith(initialState, checks)
        assert(checks.toSet.subsetOf(finalState.checks.toSet))
      }
    }

    "advancing the time into the future after initialization might expire cards, but not move them into other boxes" in {
      forAll(leitnerGens.deckState(), baseGens.nonNegativeDuration) { (initialState, duration) =>
        val newTs = initialState.timestamp.plusSeconds(duration.toSeconds)
        val DeckStateUpdate(newState, changed) = initialState.recalculateAt(newTs)
        assertThatChangedMakesSense(initialState, newState, changed)

        val movedIntoOtherBox = changed.filter { card =>
          initialState.cards(card)._2 != newState.cards(card)._2
        }

        assert(movedIntoOtherBox.isEmpty)
      }
    }

    "going back in time before the first card pushes everything into the first box with state new" in {
      forAll(leitnerGens.deckState(), baseGens.positiveDuration) { (initialState, duration) =>
        val minTs = (initialState.timestamp :: initialState.checks.map(_.timestamp)).min
        val newTs = minTs.minusNanos(duration.toNanos)
        val DeckStateUpdate(newState, changed) = initialState.recalculateAt(newTs)
        assertThatChangedMakesSense(initialState, newState, changed, timestampSetManually = true)
        assert(newState.boxes.get(BoxRef(newState.boxSpecs.head, 0)) === Some(newState.cards.keySet))
        assert(newState.cards.values.map(_._1).toSet === Set(CardState.New: CardState))
      }
    }

    "if all checks are scoring perfect and come after the initial timestamp, cards can only move up" in {
      forAll(
        leitnerGens.deckStateWithChecks(checkScores = Vector(Score.Perfect)), baseGens.nonNegativeDuration) { case ((initialState, checks), offset) =>
        assertCardsMoveInOneDirectionOnly(initialState, checks, offset, up = true)
      }
    }

    "if all checks are scoring zero and come after the initial timestamp, cards can only move down (if specs demand at least Score.Poor)" in {
      forAll(
        leitnerGens.deckStateWithChecks(checkScores = Vector(Score.Zero), minMinScore = Score.Poor), baseGens.nonNegativeDuration) { case ((initialState, checks), offset) =>
        assertCardsMoveInOneDirectionOnly(initialState, checks, offset, up = false)
      }
    }

    "if updated with checks that have already been added, cards stay put" in {
      forAll(leitnerGens.deckState()) { initialState =>
        val finalState = updateWith(initialState, initialState.checks)
        assert(finalState.cards === initialState.cards)
        assert(finalState.boxSpecs == initialState.boxSpecs)
        assert(finalState.timestamp === initialState.timestamp)
        assert(finalState.boxes === initialState.boxes)
      }
    }

    "the order in which checks are used for updates does not matter" in {
      forAll(leitnerGens.deckStateWithChecks()) { case (initialState, checks) =>
          val reorderedChecks = Random.shuffle(checks)
          val state1 = updateWith(initialState, checks)
          val state2 = updateWith(initialState, reorderedChecks)
          assert(state1.cards === state2.cards)
      }
    }
  }

  private def assertCardsMoveInOneDirectionOnly(initialState: DeckState, checks: List[Check], offset: FiniteDuration, up: Boolean): Unit = {
    val oldestCheckTs = (initialState.timestamp :: checks.map(_.timestamp)).min
    val millisDeltaToOldestDuration = initialState.timestamp.toEpochMilli - oldestCheckTs.toEpochMilli
    val timeShiftToApplyMillis = millisDeltaToOldestDuration + 1 + offset.toMillis
    val adaptedChecksSorted = {
      checks.map(check => check.copy(timestamp = check.timestamp.plusMillis(timeShiftToApplyMillis)))
        .sortBy(_.timestamp)
    }

    assertSane(initialState)
    val finalState = adaptedChecksSorted.foldLeft(initialState) { (oldState, check) =>
      val DeckStateUpdate(newState, changed) = oldState.updateWith(check)
      assertThatChangedMakesSense(oldState, newState, changed)

      val cardsThatMovedInWrongDirection = changed.filter { card =>
        val List(ind1, ind2) = List(oldState, newState)
          .map(_.cards(card)._2)
          .map(_.index)

        if (up) ind1 > ind2
        else ind1 < ind2
      }
      assert(cardsThatMovedInWrongDirection.isEmpty)

      newState
    }

    assertSane(finalState)
  }

  private def assertBoxSpecsConsistantWithBoxes(deckState: DeckState): Unit = {
    assert(deckState.boxes.keySet.map(_.spec).subsetOf(deckState.boxSpecs.toVector.toSet))
  }

  private def assertCardsConsistentWithBoxes(deckState: DeckState): Unit = {
    val cards1 = deckState.boxes.flatMap(_._2).toSet
    val cards2 = deckState.cards.keySet
    assert(cards1 === cards2)
  }

  private def assertChecksConsistentWithCards(deckState: DeckState): Unit = {
    val checkWithoutCards = deckState.checks.filter { check =>
      val card = Card(check.translation, check.direction)
      !deckState.cards.keySet.contains(card)
    }

    assert(checkWithoutCards.isEmpty)
  }

  private def assertSane(deckState: DeckState): Unit = {
    assertBoxSpecsConsistantWithBoxes(deckState)
    assertCardsConsistentWithBoxes(deckState)
    assertChecksConsistentWithCards(deckState)
  }

  private def assertTimestampAfterAllChecks(deckState: DeckState): Unit = {
    val latestTs = if (deckState.checks.isEmpty) deckState.timestamp else {
      deckState.checks.maxBy(_.timestamp).timestamp
    }

    if (latestTs != deckState.timestamp)
      assert(latestTs.isBefore(deckState.timestamp), s"latestTs: $latestTs, timestamp: ${deckState.timestamp}")
  }

  private def assertThatChangedMakesSense(oldState: DeckState, newState: DeckState, changed: Set[Card], timestampSetManually: Boolean = false): Unit = {
    assertSane(newState)

    if (!timestampSetManually)
      assertTimestampAfterAllChecks(newState)

    val actuallyChanged = {
      val cardsSet = oldState.cards.keySet ++ newState.cards.keySet
      cardsSet.filter { card =>
        oldState.cards.get(card) != newState.cards.get(card)
      }
    }

    assert(actuallyChanged === changed)
  }

  private def updateWith(initialState: DeckState, checks: List[Check]): DeckState = {
    checks.foldLeft(initialState) { (oldState, check) =>
      withClue(s"check: $check") {
        val DeckStateUpdate(newState, changed) = oldState.updateWith(check)
        assertSane(newState)
        assertThatChangedMakesSense(oldState, newState, changed)

        newState
      }
    }
  }
}
