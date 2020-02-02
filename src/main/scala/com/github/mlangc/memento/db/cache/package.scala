package com.github.mlangc.memento.db

import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex
import shapeless.{Witness => W}

package object cache {
  type CacheKeyRefinement = MatchesRegex[W.`"(?i)[0-9a-z][0-9a-z.]{0,63}"`.T]
  type CacheKey = String Refined CacheKeyRefinement
}
