package com.github.mlangc.memento.trainer.console

import java.io.File
import java.time.temporal.ChronoUnit
import java.time.LocalDateTime
import java.time.ZoneId
import java.util.Collections

import cats.instances.string._
import cats.syntax.eq._
import ciris.ConfigErrors
import com.github.difflib.text.DiffRowGenerator
import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.google.sheets.GsheetsCfg
import com.github.mlangc.memento.db.google.sheets.GsheetsVocabularyDb
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.LanguageName
import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.errors.ErrorMessage
import com.github.mlangc.memento.i18n.ConsoleMessages
import com.github.mlangc.memento.i18n.Messages
import com.github.mlangc.memento.i18n.MotivatorMessages
import com.github.mlangc.memento.trainer.VocabularyTrainer
import com.github.mlangc.memento.trainer.console.ConsoleTrainer.Reload
import com.github.mlangc.memento.trainer.examiner.DefaultExaminer
import com.github.mlangc.memento.trainer.examiner.Exam
import com.github.mlangc.memento.trainer.examiner.Examiner
import com.github.mlangc.memento.trainer.model.Feedback.Correction
import com.github.mlangc.memento.trainer.model.Answer
import com.github.mlangc.memento.trainer.model.Feedback
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.model.TechnicalIssue
import com.github.mlangc.memento.trainer.repetition.leitner.LeitnerRepetitionScheme
import com.github.mlangc.slf4zio.api.LoggingSupport
import eu.timepit.refined.auto._
import org.fusesource.jansi.Ansi.ansi
import org.fusesource.jansi.AnsiConsole.out.println
import org.jline.reader.LineReaderBuilder
import zio._
import zio.blocking.Blocking
import zio.console.Console

import scala.io.StdIn


class ConsoleTrainer private(consoleMessages: ConsoleMessages,
                             motivatorMessages: MotivatorMessages,
                             stopMessageDismissedRef: Ref[Boolean],
                             reloadingRef: Ref[Boolean])
  extends VocabularyTrainer with LoggingSupport {

  private val reader = LineReaderBuilder.builder().build()

  private val QuitCmd = ":q"
  private val HintCmd = ":?"
  private val HintCmdObsolete = "?"
  private val ReloadCmd = ":r"

  def train(db: VocabularyDb, examiner: Examiner): Task[Unit] =
    examiner.prepareExam(db).use {
      case None => Task(println(consoleMessages.noDataToTrainOn))
      case Some(exam) => clearScreen *> runExam(db, examiner, exam)
    }

  private def runExam(db: VocabularyDb, examiner: Examiner, exam: Exam): Task[Unit] = {
    def continueExam: Task[Unit] =
      exam.nextQuestion(doAsk(exam)).flatMap {
        case (question, Some(feedback)) =>
          printFeedback(question, feedback) *> eventuallyWaitForEnter(feedback) *> clearScreen *> runExam(db, examiner, exam)
        case (_, None) =>
          ZIO.whenM(reloadingRef.get) {
            reloadingRef.set(false) *> printReloadingMsg *> train(db, examiner)
          }
      }

    def continue: Task[Boolean] =
      for {
        reloading <- reloadingRef.get
        shouldStop <- exam.shouldStop
        dismissed <- stopMessageDismissedRef.get
      } yield reloading || !shouldStop || dismissed

    def askIfContinue: Task[Boolean] =
      Task {
        println(consoleMessages.allDoneContinueAnyway)
        val resp = reader.readLine(consoleMessages.promptYesNoDefaultingToNo)
        consoleMessages.isYes(resp)
      }.tap { continueExam =>
        if (continueExam) stopMessageDismissedRef.set(true)
        else Task.unit
      }

    continue.flatMap { continue =>
      if (continue) continueExam else {
        askIfContinue.flatMap {
          case true => continueExam
          case false => Task.unit
        }
      }
    }
  }

  private def eventuallyWaitForEnter(feedback: Feedback): Task[Unit] =
    if (feedback == Feedback.Postponed) Task.unit else Task {
      println()
      println(consoleMessages.pressEnterToContinue)
      StdIn.readLine()
    }.unit

  private def printFeedback(question: Question, feedback: Feedback): Task[Unit] = Task {
    feedback match {
      case Feedback.Postponed => ()
      case correction@Feedback.Correction(_, _, score) => score match {
        case Score.Perfect =>
          println()
          println(score.toString)

        case _ =>
          println()
          println(renderFeedback(question, correction))
      }
    }
  }

  private def printReloadingMsg: Task[Unit] = Task {
    println()
    println(consoleMessages.reloading)
    println()
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
      maybeIssue <- exam.technicalIssue
      prompt <- printQuestion(question, exam.language1, exam.language2, maybeIssue)
      input <- readInput(prompt)
      answer <- parseInput(input) match {
        case Right(answer) => Task.succeed(answer)
        case Left(Reload) => reloadingRef.set(true).as(None)
      }
    } yield answer

  private def printQuestion(question: Question,
                            lang1: LanguageName, lang2: LanguageName,
                            maybeIssue: Option[TechnicalIssue]): Task[String] =
    Task {
      val (knownSide, knownLang) = question.direction match {
        case Direction.LeftToRight => (question.translation.left, lang1)
        case _ => (question.translation.right, lang2)
      }

      val lastAsked: Option[LocalDateTime] = question.lastAsked.map { instant =>
        instant.atZone(ZoneId.systemDefault()).toLocalDateTime.truncatedTo(ChronoUnit.SECONDS)
      }

      println(consoleMessages.help(QuitCmd, HintCmd, ReloadCmd))
      println()

      maybeIssue.foreach { issue =>
        println(s"There seems to be a technical issue: ${issue.explanation}")
        issue.warnings.foreach { warning =>
          println(s"Warning: $warning")
        }

        println()
      }

      println(consoleMessages.timesAskedBefore(question.timesAskedBefore))
      println(consoleMessages.lastAsked(lastAsked))

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

  private def parseInput(input: String): Either[Reload.type, Option[Answer]] = input.trim match {
    case QuitCmd => Right(None)
    case ReloadCmd => Left(Reload)
    case HintCmd | HintCmdObsolete => Right(Some(Answer.NeedHint))
    case "" => Right(Some(Answer.Blank))
    case _ => Right(Some(Answer.Text(input)))
  }
}

object ConsoleTrainer extends App {
  private case object Reload

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    def tryTraining(sheetsCfg: GsheetsCfg): RIO[Blocking, Int] =
      for {
        db <- GsheetsVocabularyDb.make(sheetsCfg.sheetId, new File(sheetsCfg.credentialsPath))
        messages <- Messages.forDefaultLocale
        trainer <- make(messages.console, messages.motivator)
        _ <- trainer.train(db, new DefaultExaminer(new LeitnerRepetitionScheme))
      } yield 0

    def handleConfigErrors(errors: ConfigErrors): ZIO[Console, Nothing, Int] =
      console.putStrLn("Error loading configuration:") *>
        ZIO.foreach(errors.toVector) { error =>
          console.putStrLn("  " + error.message)
        } *> ZIO.succeed(1)

    GsheetsCfg.load match {
      case Left(configErrors) => handleConfigErrors(configErrors)

      case Right(sheetsCfg) => tryTraining(sheetsCfg).catchSome {
        case errorMessage: ErrorMessage =>
          console.putStrLn(errorMessage.getMessage) *> ZIO.succeed(1)
      }.orDie
    }
  }

  def make(consoleMessages: ConsoleMessages, motivatorMessages: MotivatorMessages): UIO[ConsoleTrainer] =
    for {
      stopMessageDismissedRef <- Ref.make(false)
      reloadRef <- Ref.make(false)
      trainer = new ConsoleTrainer(consoleMessages, motivatorMessages, stopMessageDismissedRef, reloadRef)
    } yield trainer
}
