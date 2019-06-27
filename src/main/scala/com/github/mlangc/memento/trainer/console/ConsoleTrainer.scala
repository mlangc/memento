package com.github.mlangc.memento.trainer.console

import java.io.File
import java.util.Collections

import cats.instances.string._
import cats.syntax.eq._
import com.github.difflib.text.DiffRowGenerator
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
import com.github.mlangc.memento.trainer.model.Feedback.Correction
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.repetition.leitner.LeitnerRepetitionScheme
import com.github.mlangc.memento.util.convenience.syntax.ciris._
import org.fusesource.jansi.Ansi.ansi
import org.fusesource.jansi.AnsiConsole.out.print
import org.fusesource.jansi.AnsiConsole.out.println
import org.jline.reader.LineReaderBuilder
import zio.App
import zio.Task
import zio.ZIO

import scala.io.StdIn


class ConsoleTrainer(consoleMessages: ConsoleMessages, motivatorMessages: MotivatorMessages) extends VocabularyTrainer {
  private val reader = LineReaderBuilder.builder().build()

  private val QuitCmd = ":q"
  private val HintCmd = ":?"
  private val HintCmdObsolete = "?"

  def train(db: VocabularyDb, examiner: Examiner): Task[Unit] =
    examiner.prepareExam(db).flatMap {
      case None => Task(println(consoleMessages.noDataToTrainOn))
      case Some(exam) => clearScreen *> runExam(exam)
    }

  private def runExam(exam: Exam): Task[Unit] = {
    exam.nextQuestion(doAsk(exam)).flatMap {
      case (question, Some(feedback)) => printFeedback(question, feedback) *> eventuallyWaitForEnter(feedback) *> clearScreen *> runExam(exam)
      case (_, None) => Task.unit
    }
  }

  private def eventuallyWaitForEnter(feedback: Feedback): Task[Unit] =
    if (feedback == Feedback.Postponed) Task.unit else Task {
      println()
      print(consoleMessages.pressEnterToContinue)
      StdIn.readLine()
    }.unit

  private def printFeedback(question: Question, feedback: Feedback): Task[Unit] = Task {
    feedback match {
      case Feedback.Postponed => ()
      case correction @ Feedback.Correction(_, _, score) => score match {
        case Score.Perfect =>
          println()
          println(score.toString)

        case _ =>
          println()
          println(renderFeedback(question, correction))
      }
    }
  }

  private def renderFeedback(question: Question, correction: Correction): String = {
    def diffGen: DiffRowGenerator = DiffRowGenerator
      .create()
      .showInlineDiffs(true)
      .mergeOriginalRevised(true)
      .oldTag(open => (if (open) ansi().fgRed() else ansi().fgDefault()).toString)
      .newTag(open => (if (open) ansi().fgGreen() else ansi().fgDefault()).toString)
      .build()

    def diff = diffGen.generateDiffRows(
      Collections.singletonList(correction.got),
      Collections.singletonList(correction.expected.spelling.value)
    ).get(0).getOldLine

    val expected = correction.expected.spelling.value
    val percentageRevealed = (question.revealed.value * 100).round.toInt

    if (expected === correction.got) {
      consoleMessages.correctAnswerWithScore(percentageRevealed, correction.score)
    } else if (question.revealed.value > 0) {
      consoleMessages.wrongAnswerWithScoreRevealed(
        expected, correction.got, diff, percentageRevealed, correction.score)
    } else {
      consoleMessages.wrongAnswerWithScore(expected, correction.got, diff, correction.score)
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

    println(consoleMessages.help(QuitCmd, HintCmd))
    println()
    println(consoleMessages.timesAskedBefore(question.timesAskedBefore))
    println(consoleMessages.lastAsked(question.lastAsked))

    println()
    question.motivators.foreach { motivator =>
      println(motivator.text(motivatorMessages))
    }

    println()
    question.hint.foreach { hint =>
      println(consoleMessages.hint(hint.spelling))
      println()
    }

    s"$knownLang[${knownSide.spelling}] ---> "
  }

  private def readInput(prompt: String): Task[String] = Task {
    reader.readLine(prompt)
  }

  private def clearScreen: Task[Unit] = Task {
    println(ansi().cursor(0, 0).eraseScreen())
  }

  private def parseInput(input: String): Option[Answer] = input.trim match {
    case QuitCmd => None
    case HintCmd | HintCmdObsolete => Some(Answer.NeedHint)
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
