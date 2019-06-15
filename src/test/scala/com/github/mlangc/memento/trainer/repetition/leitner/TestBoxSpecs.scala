package com.github.mlangc.memento.trainer.repetition.leitner

import cats.data.NonEmptyVector
import com.github.mlangc.memento.db.model.Score

import scala.concurrent.duration._

private[leitner] object TestBoxSpecs {
  val defaultBoxSpecs: NonEmptyVector[BoxSpec] = NonEmptyVector.of(
    BoxSpec(5.seconds, Score.Poor),
    BoxSpec(25.seconds, Score.Poor),
    BoxSpec(2.minutes, Score.SoSo),
    BoxSpec(10.minutes, Score.SoSo),
    BoxSpec(1.hour, Score.SoSo),
    BoxSpec(5.hours, Score.SoSo),
    BoxSpec(1.day, Score.Good),
    BoxSpec(5.days, Score.Good),
    BoxSpec(25.days, Score.Perfect),
    BoxSpec(120.days, Score.Perfect),
    BoxSpec((365 * 2).days, Score.Perfect)
  )

  val simpleBoxSpecs: NonEmptyVector[BoxSpec] = NonEmptyVector.of(
    BoxSpec(5.seconds, Score.Poor),
    BoxSpec(25.seconds, Score.Poor),
    BoxSpec(1.day, Score.SoSo),
    BoxSpec(2.days, Score.Good)
  )
}
