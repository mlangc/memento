package com.github.mlangc.memento.trainer.model

sealed trait Answer
sealed trait ScorableAnswer extends Answer

object Answer {

  case object Blank extends ScorableAnswer
  case class Text(value: String) extends ScorableAnswer

  case object NeedHint extends Answer
}
