package com.github.mlangc.memento.trainer.repetition

import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.trainer.model.Card

trait RepetitionStrategy {
  def implement(cards: Set[Card], checks: List[Check]): RepetitionStrategy.Impl
}

object RepetitionStrategy {
  type Propability = Double

  trait Impl {
    def dist: Map[Card, Propability]
    def status: RepetitionStatus
    def update(check: Check): Impl
  }
}
