package com.github.mlangc.memento.db.model

import java.time.Instant

import cats.Order

case class Check(translation: Translation, direction: Direction, score: Score, timestamp: Instant)

object Check {
  implicit val checkOrdering: Ordering[Check] = Ordering.by(c => (c.timestamp, c.translation, c.direction, c.score))
  implicit val checkOrder: Order[Check] = Order.fromOrdering
}
