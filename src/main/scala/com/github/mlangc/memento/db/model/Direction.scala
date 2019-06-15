package com.github.mlangc.memento.db.model

import com.github.mlangc.memento.db.model.Direction.LeftToRight
import com.github.mlangc.memento.db.model.Direction.RightToLeft

sealed trait Direction {
  def flip: Direction = this match {
    case LeftToRight => RightToLeft
    case RightToLeft => LeftToRight
  }
}

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
