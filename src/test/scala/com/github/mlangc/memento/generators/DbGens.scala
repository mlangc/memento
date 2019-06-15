package com.github.mlangc.memento.generators

import cats.data.NonEmptyVector
import com.github.mlangc.memento.db.model._
import org.scalacheck.Gen

class DbGens(val baseGens: BaseGens = new BaseGens()) {
  def score: Gen[Score] = Gen.oneOf(Score.values)

  def word: Gen[Word] = Gen.alphaStr.map(Word.apply)

  def translation: Gen[Translation] =
    for {
      word1 <- word
      word2 <- word
    } yield Translation(word1, word2)

  def translations: Gen[NonEmptyVector[Translation]] =
    for {
      t <- translation
      ts <- Gen.listOf(translation).map(_.toVector)
    } yield NonEmptyVector(t, ts)

  def direction: Gen[Direction] = Gen.oneOf(Direction.LeftToRight, Direction.RightToLeft)

  def check(translations: NonEmptyVector[Translation], scores: IndexedSeq[Score] = Score.values): Gen[Check] =
    for {
      trans <- Gen.oneOf(translations.toVector)
      ts <- baseGens.instant
      dir <- direction
      sc <- Gen.oneOf(scores)
    } yield Check(trans, dir, sc, ts)

  def checks(translations: NonEmptyVector[Translation], scores: IndexedSeq[Score] = Score.values): Gen[List[Check]] =
    Gen.listOf(check(translations, scores))

}
