package com.github.mlangc.memento.db

import com.softwaremill.tagging.@@

package object model {
  trait LanguageNameTag
  type LanguageName = String @@ LanguageNameTag
}
