package com.github.mlangc.memento.db.google

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.numeric.Positive

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
}
