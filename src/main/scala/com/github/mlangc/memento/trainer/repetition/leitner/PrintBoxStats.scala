package com.github.mlangc.memento.trainer.repetition.leitner

import java.io.File
import java.time.Instant

import cats.data.{NonEmptyList, NonEmptyVector}
import com.github.mlangc.memento.db.google.sheets.{GsheetsCfg, GsheetsVocabularyDb}
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Direction.{LeftToRight, RightToLeft}
import com.github.mlangc.memento.trainer.model.TrainingData
import com.github.mlangc.memento.util.convenience.syntax.ciris._
import scalaz.zio.console.Console
import scalaz.zio.{App, UIO, ZIO, console}


object PrintBoxStats extends App {
  private val boxSpecs = BoxSpecs.defaultBoxSpecs

  override def run(args: List[String]): ZIO[PrintBoxStats.Environment, Nothing, Int] = {
    for {
      sheetId <- GsheetsCfg.sheetId.orDie
      credPath <- GsheetsCfg.credentialsPath.orDie
      db <- GsheetsVocabularyDb.make(sheetId, new File(credPath))
      trainingData <- db.load.map(TrainingData.convert)
      now <- UIO(Instant.now())
      maybeDeckState = NonEmptyVector.fromVector(trainingData.translations).map { translations =>
        NonEmptyList.fromList(trainingData.checks)
          .map(checks => DeckState.init(translations, boxSpecs, checks))
          .getOrElse(DeckState.init(translations, boxSpecs, now))
          .recalculateAt(now).newState
      }

      _ <- maybeDeckState match {
        case Some(deckState) => prettyPrint(deckState)
        case None => console.putStrLn("No translations - nothing to do")
      }
    } yield 0
  }.orDie

  private def prettyPrint(deckState: DeckState): ZIO[Console, Nothing, Unit] =
    console.putStrLn(render(deckState))

  private def render(deckState: DeckState): String = {
    def getState(card: Card): CardState = deckState.cards(card)._1

    def renderCard(card: Card): String = {
      s"${getState(card)} -- ${card.translation.left.spelling} ${toArrow(card.direction)} ${card.translation.right.spelling}"
    }

    def renderBox(boxRef: BoxRef): String = {
      val header = s"""Box(${boxRef.index}, ${boxRef.spec.interval}, ${boxRef.spec.minScore}):"""

      val cards = deckState.boxes(boxRef).toList
        .sortBy(card => (getState(card).shouldBeTested, getState(card).entryName, card.translation.left.spelling.value, card.direction.toString))

      val body = cards
        .map(renderCard).map("  " + _)
        .mkString("", "\n", "")

      val footer = s"  ${cards.size} cards in total"

      List(header, body, footer).mkString("\n")
    }

    val header =
      s"""DeckState@${deckState.timestamp}:
         |  ${deckState.cards.keySet.size} cards, ${deckState.checks.size} checks""".stripMargin

    val body = deckState.boxes.keysIterator
      .toSeq.sortBy(_.index)
      .map(renderBox)
      .mkString("\n\n")

    List(header, body).mkString("\n\n")
  }


  private def toArrow(direction: Direction): String = direction match {
    case LeftToRight => "-->"
    case RightToLeft => "<--"
  }
}
