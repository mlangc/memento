package com.github.mlangc.memento.generators

import java.time.Instant
import java.util.concurrent.TimeUnit

import cats.data.{NonEmptyList, NonEmptyVector}
import org.scalacheck.Gen

import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

class BaseGens(val minInstant: Instant = Instant.EPOCH,
               val maxInstant: Instant = Instant.EPOCH.plusSeconds(2L * 3600L * 24L * 365L)) {
  assert(minInstant.isBefore(maxInstant))

  def instant: Gen[Instant] = {
    val secs = maxInstant.getEpochSecond - minInstant.getEpochSecond
    Gen.chooseNum(0, secs).map(Instant.ofEpochSecond)
  }

  def nonEmptyListOf[T](gen: Gen[T]): Gen[NonEmptyList[T]] =
    Gen.nonEmptyListOf(gen).map(l => NonEmptyList(l.head, l.tail))

  def nonEmptyVectorOf[T](gen: Gen[T]): Gen[NonEmptyVector[T]] =
    Gen.nonEmptyListOf(gen).map(l => NonEmptyVector(l.head, l.tail.toVector))

  def nonNegativeDuration: Gen[FiniteDuration] = Gen.finiteDuration.suchThat(_ >= Duration.Zero)

  def positiveDuration: Gen[FiniteDuration] = Gen.finiteDuration.suchThat(_ >= Duration(1, TimeUnit.NANOSECONDS))
}
