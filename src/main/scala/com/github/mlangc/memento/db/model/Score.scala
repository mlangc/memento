package com.github.mlangc.memento.db.model

import cats.instances.int._
import cats.kernel.Order
import enumeratum._

import scala.collection.immutable

sealed abstract class Score(val ordinal: Int) extends EnumEntry

object Score extends Enum[Score] {
  case object Perfect extends Score(4)
  case object Good extends Score(3)
  case object SoSo extends Score(2)
  case object Poor extends Score(1)
  case object Zero extends Score(0)

  val values: immutable.IndexedSeq[Score] = findValues

  def parse(str: String): Option[Score] = withNameInsensitiveOption(str)

  implicit val scoreOrder: Order[Score] = Order.by[Score, Int](_.ordinal)
}
