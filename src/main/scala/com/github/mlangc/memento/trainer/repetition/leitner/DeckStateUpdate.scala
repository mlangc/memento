package com.github.mlangc.memento.trainer.repetition.leitner

private[leitner] case class DeckStateUpdate(newState: DeckState, affectedCards: Set[Card])
