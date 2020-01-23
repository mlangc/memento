package com.github.mlangc.memento.db.google

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty

package object sheets {
  type SheetIdRefinement = NonEmpty
  type SheetId = String Refined SheetIdRefinement

  type CredentialsPathRefinement = NonEmpty
  type CredentialsPath = String Refined CredentialsPathRefinement
}
