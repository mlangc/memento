package com.github.mlangc.memento.trainer.model

sealed trait Answer

sealed trait ScorableAnswer extends Answer {
  def value: String
}

object Answer {

  case object Blank extends ScorableAnswer {
    def value = ""
  }

  case class Text(value: String) extends ScorableAnswer

  case object NeedHint extends Answer
}
