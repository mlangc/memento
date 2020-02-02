package com.github.mlangc.memento.trainer.repetition.leitner

import java.time.Instant

import cats.data.NonEmptyList
import cats.data.NonEmptyVector
import com.github.mlangc.memento.db.google.sheets.GsheetsCfg
import com.github.mlangc.memento.db.google.sheets.GsheetsVocabularyDb
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Direction.LeftToRight
import com.github.mlangc.memento.db.model.Direction.RightToLeft
import com.github.mlangc.memento.trainer.model.Card
import com.github.mlangc.memento.trainer.model.TrainingData
import com.github.mlangc.memento.zenvs.ZEnvs
import eu.timepit.refined.auto._
import zio.App
import zio.UIO
import zio.ZEnv
import zio.ZIO
import zio.console
import zio.console.Console


object PrintBoxStats extends App {
  private type Resistance = Int
  private val boxSpecs = BoxSpecs.defaultBoxSpecs

  override def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    for {
      config <- GsheetsCfg.load.orDieWith(errors => new IllegalArgumentException("" + errors))
      db <- GsheetsVocabularyDb.make(config)
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
    }.orDie.provide(ZEnvs.live)

  private def prettyPrint(deckState: DeckState): ZIO[Console, Nothing, Unit] =
    console.putStrLn(render(deckState))

  private def render(deckState: DeckState): String = {
    def getState(card: Card): CardState = deckState.cards(card)._1

    val resistances = calcResistances(deckState)

    def renderCard(card: Card): String = {
      s"${getState(card)}[r${resistances(card)}] -- ${card.translation.left.spelling} ${toArrow(card.direction)} ${card.translation.right.spelling}"
    }

    def renderBox(boxRef: BoxRef): String = {
      val header = s"""Box(${boxRef.index}, ${boxRef.spec.interval}, ${boxRef.spec.minScore}):"""

      val cards = deckState.boxes(boxRef).toList
        .sortBy(card => (getState(card).shouldBeTested, -resistances(card), getState(card).entryName, card.translation.left.spelling.value, card.direction.toString))

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

  private def calcResistances(deckState: DeckState): Map[Card, Resistance] =
    deckState.cards.map { case (card, (_, BoxRef(_, i))) =>
      card -> {
        val trained = deckState.checks.count(card.correspondsTo)
        (trained.toDouble / (i + 1)).round.toInt
      }
    }
}
