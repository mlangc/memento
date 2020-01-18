package com.github.mlangc.memento.db.cache

import cats.syntax.contravariant._

import eu.timepit.refined.refineV


object BasicKeyables {
  implicit val cacheKeyKeyable: Keyable[CacheKey] = new Keyable[CacheKey] {
    def toKey(a: CacheKey): CacheKey = a
  }

  implicit val intKeyable: Keyable[Int] = {
    cacheKeyKeyable.contramap(i => refineV[CacheKeyRefinement](i.toString).toOption.get)
  }
}
