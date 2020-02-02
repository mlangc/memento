package com.github.mlangc.memento.db.cache

import com.github.mlangc.memento.BaseTest
import com.github.mlangc.memento.db.model.TestChecks
import eu.timepit.refined.auto._

import BasicCachables._

class BasicCachablesTest extends BaseTest {
  "Iterable[Check]" in {
    val checks = Iterable(
      TestChecks.VerbreitetRepandu,
      TestChecks.IchJe,
      TestChecks.LausPou)

    CachablesTestHelpers.testRoundTrip(checks)
  }
}
