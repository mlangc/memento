package com.github.mlangc.memento.i18n

import java.util.Locale

import com.github.mlangc.memento.i18n.de.DeConsoleMessages
import com.github.mlangc.memento.i18n.de.DeMotivatorMessages
import com.github.mlangc.memento.i18n.en.EnConsoleMessages
import com.github.mlangc.memento.i18n.en.EnMotivatorMessages
import zio.UIO

case class Messages private (console: ConsoleMessages, motivator: MotivatorMessages)

object Messages {
  def forDefaultLocale: UIO[Messages] =
    for {
      locale <- UIO(Locale.getDefault)
    } yield {
      locale.getLanguage match {
        case "de" => Messages(new DeConsoleMessages, new DeMotivatorMessages)
        case _ => Messages(new EnConsoleMessages, new EnMotivatorMessages)
      }
    }
}