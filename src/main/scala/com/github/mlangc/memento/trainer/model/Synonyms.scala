package com.github.mlangc.memento.trainer.model

import com.github.mlangc.memento.db.model.Synonym
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.db.model.Vocabulary

import com.github.mlangc.memento.util.std.collection.set._

case class Synonyms(left: Map[Vocabulary, Set[Vocabulary]], right: Map[Vocabulary, Set[Vocabulary]])

object Synonyms {
  val None = Synonyms(left = Map(), right = Map())

  def from(translations: List[Translation], synonyms1: List[Synonym], synonyms2: List[Synonym]): Synonyms = {
    val (syns1FromTranslations, syns2FromTranslations) = synonymsFromTranslations(translations)
    
    val synsWoOrphans: List[Set[Synonym]] = List(dropOrphans(synonyms1, translations.map(_.left)), dropOrphans(synonyms2, translations.map(_.right)))
    val synsFromTranslations: List[Set[Synonym]] = List(syns1FromTranslations, syns2FromTranslations)
    
    val List(syns1, syns2) = synsWoOrphans.zip(synsFromTranslations)
      .map { case (s1, s2) => s1.union(s2) }
      .map(deriveSyns)
      .map(_.withDefaultValue(Set.empty))

    Synonyms(syns1, syns2)
  }

  private def dropOrphans(syns: List[Synonym], vocs: List[Vocabulary]): Set[Synonym] = {
    val vocsSet = vocs.toSet
    syns.filter(s => asSet(s).intersect(vocsSet).nonEmpty).toSet
  }

  private def synonymsFromTranslations(translations: List[Translation]): (Set[Synonym], Set[Synonym]) = {
    def synsFor(left: Translation => Vocabulary, right: Translation => Vocabulary): Set[Synonym] = {
      translations.groupBy(right).values.map(_.map(left)).flatMap {
        case w1 :: ws => ws.map(w2 => Synonym(w1, w2))
        case _ => Nil
      }.toSet
    }

    synsFor(_.left, _.right) -> synsFor(_.right, _.left)
  }

  private def deriveSyns(syns: Set[Synonym]): Map[Vocabulary, Set[Vocabulary]] = {
    val connectedSyns: Set[Set[Synonym]] = syns.connectedSets { (syn1, syn2) =>
      asSet(syn1).intersect(asSet(syn2)).nonEmpty
    }

    connectedSyns.flatMap { synGroup =>
      val vocs = synGroup.flatMap(asSet)
      vocs.map(voc => voc -> (vocs - voc))
    }.toMap
  }

  private def asSet(syn: Synonym) = Set(syn.voc1, syn.voc2)
}
