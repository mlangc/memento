package com.github.mlangc.memento.trainer.repetition.leitner

import com.github.mlangc.memento.trainer.repetition.leitner.CardState.Downgraded
import com.github.mlangc.memento.trainer.repetition.leitner.CardState.Expired
import com.github.mlangc.memento.trainer.repetition.leitner.CardState.New
import enumeratum.Enum
import enumeratum.EnumEntry

import scala.collection.immutable

private[leitner] sealed trait CardState extends EnumEntry {
  def shouldBeTested: Boolean = this match {
    case New | Expired | Downgraded => true
    case _ => false
  }
}

private[leitner] object CardState extends Enum[CardState] {
  case object Dormant extends CardState
  case object Expired extends CardState
  case object Downgraded extends CardState
  case object New extends CardState

  val values: immutable.IndexedSeq[CardState] = findValues
}
