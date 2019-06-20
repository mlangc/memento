package com.github.mlangc.memento.db

import com.softwaremill.tagging.@@
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.string.Trimmed

package object model {
  trait LanguageNameTag
  type LanguageName = String @@ LanguageNameTag

  type SpellingRefinement = NonEmpty And Trimmed
  type Spelling = String Refined SpellingRefinement
}
