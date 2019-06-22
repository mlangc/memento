package com.github.mlangc.memento.trainer.console

import java.io.File

import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.google.sheets.GsheetsCfg
import com.github.mlangc.memento.db.google.sheets.GsheetsVocabularyDb
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.LanguageName
import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.i18n.ConsoleMessages
import com.github.mlangc.memento.i18n.Messages
import com.github.mlangc.memento.i18n.MotivatorMessages
import com.github.mlangc.memento.trainer.VocabularyTrainer
import com.github.mlangc.memento.trainer.examiner.DefaultExaminer
import com.github.mlangc.memento.trainer.examiner.Exam
import com.github.mlangc.memento.trainer.examiner.Examiner
import com.github.mlangc.memento.trainer.model.Answer
import com.github.mlangc.memento.trainer.model.Feedback
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.repetition.leitner.LeitnerRepetitionScheme
import com.github.mlangc.memento.util.convenience.syntax.ciris._
import org.fusesource.jansi.Ansi.ansi
import org.fusesource.jansi.AnsiConsole.out.print
import org.fusesource.jansi.AnsiConsole.out.println
import org.jline.reader.LineReaderBuilder
import scalaz.zio.App
import scalaz.zio.Task
import scalaz.zio.ZIO

import scala.io.StdIn


class ConsoleTrainer(consoleMessages: ConsoleMessages, motivatorMessages: MotivatorMessages) extends VocabularyTrainer {
  private val reader = LineReaderBuilder.builder().build()

  def train(db: VocabularyDb, examiner: Examiner): Task[Unit] =
    examiner.prepareExam(db).flatMap {
      case None => Task(println(consoleMessages.noDataToTrainOn))
      case Some(exam) => clearScreen *> runExam(exam)
    }

  private def runExam(exam: Exam): Task[Unit] = {
    exam.nextQuestion(doAsk(exam)).flatMap {
      case Some(feedback) => printFeedback(feedback) *> eventuallyWaitForEnter(feedback) *> clearScreen *> runExam(exam)
      case None => Task.unit
    }
  }

  private def eventuallyWaitForEnter(feedback: Feedback): Task[Unit] =
    if (feedback == Feedback.Postponed) Task.unit else Task {
      print(consoleMessages.pressEnterToContinue)
      StdIn.readLine()
    }.unit

  private def printFeedback(feedback: Feedback): Task[Unit] = Task {
    feedback match {
      case Feedback.Postponed => ()
      case Feedback.Correction(_, score) => score match {
        case Score.Perfect =>
          println(score.toString)

        case _ =>
          println(feedback.toString)
      }
    }
  }

  private def doAsk(exam: Exam)(question: Question): Task[Option[Answer]] =
    for {
      prompt <- printQuestion(question, exam.language1, exam.language2)
      input <- readInput(prompt)
      answer = parseInput(input)
    } yield answer

  private def printQuestion(question: Question, lang1: LanguageName, lang2: LanguageName): Task[String] = Task {
    val (knownSide, knownLang) = question.direction match {
      case Direction.LeftToRight => (question.translation.left, lang1)
      case _ => (question.translation.right, lang2)
    }

    val hintStr = question.hint
      .map(hint => s"[${consoleMessages.hint(hint.spelling)}] ")
      .getOrElse("")

    println(consoleMessages.timesAskedBefore(question.timesAskedBefore))
    println(consoleMessages.lastAsked(question.lastAsked))

    println()
    question.motivators.foreach { motivator =>
      println(motivator.text(motivatorMessages))
    }

    println()
    s"$knownLang[${knownSide.spelling}] --$hintStr--> "
  }

  private def readInput(prompt: String): Task[String] = Task {
    reader.readLine(prompt)
  }

  private def clearScreen: Task[Unit] = Task {
    println(ansi().cursor(0, 0).eraseScreen())
  }

  private def parseInput(input: String): Option[Answer] = input.trim match {
    case ":q" => None
    case "?" => Some(Answer.NeedHint)
    case "" => Some(Answer.Blank)
    case _ => Some(Answer.Text(input))
  }
}

object ConsoleTrainer extends App {
  def run(args: List[String]): ZIO[Environment, Nothing, Int] = {

    (for {
      sheetId <- GsheetsCfg.sheetId.orDie
      credentialsPath <- GsheetsCfg.credentialsPath.orDie
      db <- GsheetsVocabularyDb.make(sheetId, new File(credentialsPath))
      messages <- Messages.forDefaultLocale
      trainer = new ConsoleTrainer(messages.console, messages.motivator)
      _ <- trainer.train(db, new DefaultExaminer(new LeitnerRepetitionScheme))
    } yield 0).orDie
  }
}
