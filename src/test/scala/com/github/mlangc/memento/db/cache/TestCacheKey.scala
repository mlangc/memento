package com.github.mlangc.memento.db.cache

import com.github.mlangc.memento.BaseTest

import eu.timepit.refined.refineV

class TestCacheKey extends BaseTest {
  "Keys must not start with ." in {
    assert(refineV[CacheKeyRefinement](".test").isLeft)
  }
}
