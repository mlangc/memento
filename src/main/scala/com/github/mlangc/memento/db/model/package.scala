package com.github.mlangc.memento.db

import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.string.Trimmed

package object model {
  type LanguageNameRefinement = NonEmpty And Trimmed
  type LanguageName = String Refined LanguageNameRefinement

  type SpellingRefinement = NonEmpty And Trimmed
  type Spelling = String Refined SpellingRefinement
}
