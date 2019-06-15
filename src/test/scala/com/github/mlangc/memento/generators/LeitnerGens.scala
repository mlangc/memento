package com.github.mlangc.memento.generators

import cats.data.{NonEmptyList, NonEmptyVector}
import com.github.mlangc.memento.db.model.{Check, Score, Translation}
import com.github.mlangc.memento.trainer.repetition.leitner.{BoxSpec, DeckState}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

import scala.concurrent.duration._
import cats.syntax.order._

class LeitnerGens(val dbGens: DbGens = new DbGens()) {
  def baseGens: BaseGens = dbGens.baseGens

  def boxSpec(minMinScore: Score = Score.Zero): Gen[BoxSpec] =
    for {
      minScore <- Gen.oneOf(Score.values.filter(_ >= minMinScore))
      interval <- Gen.chooseNum(1L, 3600L * 24L * 7L).map(_.seconds)
    } yield BoxSpec(interval, minScore)

  def boxSpecs(minMinScore: Score = Score.Zero): Gen[NonEmptyVector[BoxSpec]] =
    baseGens.nonEmptyVectorOf(boxSpec(minMinScore))

  def deckState(minMinScore: Score = Score.Zero): Gen[DeckState] = deckStateWithTranslations(minMinScore).map(_._1)

  def deckStateWithTranslations(minMinScore: Score = Score.Zero): Gen[(DeckState, NonEmptyVector[Translation])] =
    for {
      translations <- dbGens.translations
      withoutInitialChecks <- arbitrary[Boolean]
      specs <- boxSpecs(minMinScore)
      deckState <- {
        if (withoutInitialChecks) {
          for {
            c <- dbGens.check(translations)
            cs <- dbGens.checks(translations)
            checks = NonEmptyList(c, cs)
          } yield DeckState.init(translations, specs, checks)
        } else {
          baseGens.instant.map(DeckState.init(translations, specs, _))
        }
      }
    } yield (deckState, translations)

  def deckStateWithChecks(minMinScore: Score = Score.Zero,
                          checkScores: IndexedSeq[Score] = Score.values)
  : Gen[(DeckState, List[Check])] =
    for {
      (deckState, translations) <- deckStateWithTranslations(minMinScore)
      checks <- dbGens.checks(translations, checkScores)
    } yield (deckState, checks)
}
