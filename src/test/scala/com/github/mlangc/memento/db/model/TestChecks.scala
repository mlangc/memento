package com.github.mlangc.memento.db.model

import java.time.Instant

import eu.timepit.refined.auto._

object TestChecks {
  val IchJe = Check(Translation("ich", "je"), Direction.LeftToRight, Score.Perfect, Instant.EPOCH)
  val VerbreitetRepandu = Check(Translation("verbreitet", "répandu"), Direction.LeftToRight, Score.Perfect, Instant.EPOCH)
  val LausPou = Check(Translation("Laus", "un pou"), Direction.RightToLeft, Score.SoSo, Instant.EPOCH.plusSeconds(3600))
}
