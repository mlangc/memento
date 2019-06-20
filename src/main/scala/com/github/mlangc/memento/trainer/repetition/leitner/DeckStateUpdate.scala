package com.github.mlangc.memento.trainer.repetition.leitner

import com.github.mlangc.memento.trainer.model.Card

private[leitner] case class DeckStateUpdate(newState: DeckState, affectedCards: Set[Card])
