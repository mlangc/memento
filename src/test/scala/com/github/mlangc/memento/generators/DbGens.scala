package com.github.mlangc.memento.generators

import cats.data.{NonEmptyList, NonEmptyVector}
import cats.syntax.foldable._
import com.github.mlangc.memento.db.model._
import eu.timepit.refined.refineV
import org.scalacheck.Gen

class DbGens(val baseGens: BaseGens = new BaseGens()) {
  def score: Gen[Score] = Gen.oneOf(Score.values)

  def spelling: Gen[Spelling] =
    for {
      n <- Gen.chooseNum(1, 3)
      word <- Gen.chooseNum(1,15)
        .flatMap(l => Gen.listOfN(l, Gen.alphaChar))
        .map(_.mkString)

      spelling <- Gen.listOfN(n, word)
        .map(_.mkString(" "))
        .map(s => refineV[SpellingRefinement](s).toOption)
        .suchThat(_.nonEmpty)
        .map(_.get)
    } yield spelling

  def vocabulary: Gen[Vocabulary] = spelling.map(Vocabulary.apply)

  def translation: Gen[Translation] =
    for {
      voc1 <- vocabulary
      voc2 <- vocabulary
    } yield Translation(voc1, voc2)

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

  def synonym(vocabulary: NonEmptyList[Vocabulary]): Gen[Synonym] =
    for {
      voc1 <- Gen.oneOf(vocabulary.toList)
      voc2 <- Gen.oneOf(vocabulary.toList)
    } yield Synonym(voc1, voc2)

  def synonyms(vocabulary: List[Vocabulary]): Gen[List[Synonym]] = NonEmptyList.fromList(vocabulary) match {
    case Some(vocs) => Gen.listOf(synonym(vocs))
    case None => Gen.const(Nil)
  }

  def languageName: Gen[LanguageName] =
    Gen.alphaStr.asInstanceOf[Gen[LanguageName]]

  def vocabularyData: Gen[VocabularyData] =
    for {
      lang1 <- languageName
      lang2 <- languageName.suchThat(_ != lang1)
      trans <- translations
      syns1 <- synonyms(trans.map(_.left).toList)
      syns2 <- synonyms(trans.map(_.right).toList)
      chs <- checks(trans)
    } yield VocabularyData(lang1, lang2, trans.toList, syns1, syns2, chs)
}
