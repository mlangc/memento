package com.github.mlangc.memento.db.cache

import org.scalatest.Assertions._

object CachablesTestHelpers {
  def testRoundTrip[A](a: A)(implicit cachable: Cachable[A]): Unit = {
    val bytes = cachable.toBytes(a)
    val a2 = cachable.fromBytes(bytes)
    assert(a2 == Right(a))
    ()
  }

  def testRoundTrips[A](as: Iterable[A])(implicit cachable: Cachable[Iterable[A]]): Unit = {
    as.inits.foreach { as =>
      withClue(as)(testRoundTrip(as))
    }
  }
}
