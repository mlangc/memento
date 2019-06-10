package com.github.mlangc.memento.db.model

import java.time.Instant

case class Check(translation: Translation, direction: Direction, score: Score, timestamp: Instant)
