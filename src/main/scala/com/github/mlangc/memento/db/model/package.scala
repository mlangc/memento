package com.github.mlangc.memento.db

import com.softwaremill.tagging.@@

package object model {
  trait WordIdTag
  type WordId = Long @@ WordIdTag

  trait TranslationIdTag
  type TranslationId = Long @@ TranslationIdTag

  trait CheckIdTag
  type CheckId = Long @@ CheckIdTag

  trait LanguageNameTag
  type LanguageName = String @@ LanguageNameTag
}
