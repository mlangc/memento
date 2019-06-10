package com.github.mlangc.memento.trainer.model

import com.github.mlangc.memento.db.model.Word

case class Synonyms(left: Map[Word, Set[Word]], right: Map[Word, Set[Word]])

object Synonyms {
  val None = Synonyms(left = Map(), right = Map())
}
