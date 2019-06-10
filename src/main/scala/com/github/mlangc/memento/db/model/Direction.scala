package com.github.mlangc.memento.db.model

sealed trait Direction

object Direction {
  case object LeftToRight extends Direction
  case object RightToLeft extends Direction

  def fromBoolean(b: Boolean): Direction =
    if(b) LeftToRight else RightToLeft

  def fromString(s: String): Option[Direction] = s match {
    case "LeftToRight" => Some(LeftToRight)
    case "RightToLeft" => Some(RightToLeft)
    case _ => None
  }
}
