package com.github.mlangc.memento.db.model

import enumeratum._

sealed trait Score extends EnumEntry

object Score extends Enum[Score] {
  case object Perfect extends Score
  case object Good extends Score
  case object SoSo extends Score
  case object Poor extends Score
  case object Zero extends Score

  val values = findValues

  def parse(str: String): Option[Score] = withNameInsensitiveOption(str)
}
