package com.github.mlangc.memento.trainer.model

import com.github.mlangc.memento.db.model.Synonym
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.db.model.Vocabulary

case class Synonyms(left: Map[Vocabulary, Set[Vocabulary]], right: Map[Vocabulary, Set[Vocabulary]])

object Synonyms {
  val None = Synonyms(left = Map(), right = Map())

  def from(translations: List[Translation], synonyms1: List[Synonym], synonyms2: List[Synonym]): Synonyms = {
    Synonyms.None
  }

  private def synonymsFromTranslations(translations: List[Translation]): (Set[Synonym], Set[Synonym]) = {
    def synsFor(left: Translation => Vocabulary, right: Translation => Vocabulary): Set[Synonym] = {
      translations.groupBy(right).mapValues(_.map(left)).flatMap { case (v, ws) =>
        ws.flatMap { w =>
          if (v == w) Nil
          else List(Synonym(v, w))
        }
      }
    }.toSet

    synsFor(_.left, _.right) -> synsFor(_.right, _.left)
  }

}
