package com.github.mlangc.memento.db.google

import com.github.mlangc.memento.BaseZioTest
import org.scalatest.Outcome

abstract class BaseGapiTest extends BaseZioTest {
  protected override def withFixture(test: NoArgTest): Outcome = {
    val envVar = "RUN_GAPI_TESTS"
    if (System.getenv(envVar) == "true") test() else {
      cancel(s"Cancelling GAPI related tests; set the environment variable $envVar to true to enable them")
    }
  }
}
