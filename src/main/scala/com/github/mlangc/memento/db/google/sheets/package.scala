package com.github.mlangc.memento.db.google

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.string.MatchesRegex
import shapeless.{Witness => W}

package object sheets {
  type SheetIdRefinement = NonEmpty
  type SheetId = String Refined SheetIdRefinement

  type TokensPathRefinement = NonEmpty
  type TokensPath = String Refined TokensPathRefinement

  type CachePathRefinement = NonEmpty
  type CachePath = String Refined CachePathRefinement

  type MetadataIdRefinement = Positive
  type MetadataId = Int Refined MetadataIdRefinement

  type SchemaVersionRefinement = NonNegative
  type SchemaVersion = Int Refined SchemaVersionRefinement

  type Sha1Refinement = MatchesRegex[W.`"[a-z0-9]{40}"`.T]
  type Sha1 = String Refined Sha1Refinement
}
