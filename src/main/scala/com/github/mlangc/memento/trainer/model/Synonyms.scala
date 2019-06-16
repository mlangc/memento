package com.github.mlangc.memento.trainer.model

import com.github.mlangc.memento.db.model.Vocabulary

case class Synonyms(left: Map[Vocabulary, Set[Vocabulary]], right: Map[Vocabulary, Set[Vocabulary]])

object Synonyms {
  val None = Synonyms(left = Map(), right = Map())
}
