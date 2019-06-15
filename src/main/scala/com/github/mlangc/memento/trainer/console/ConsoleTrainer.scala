package com.github.mlangc.memento.trainer.console

import java.io.File

import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.google.sheets.{GsheetsCfg, GsheetsVocabularyDb}
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.LanguageName
import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.trainer.VocabularyTrainer
import com.github.mlangc.memento.trainer.examiner.{DefaultExaminer, Exam, Examiner}
import com.github.mlangc.memento.trainer.model.Answer
import com.github.mlangc.memento.trainer.model.Feedback
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.repetition.leitner.LeitnerRepetitionScheme
import scalaz.zio.App
import scalaz.zio.Task
import scalaz.zio.ZIO
import com.github.mlangc.memento.util.convenience.syntax.ciris._

import scala.io.StdIn

class ConsoleTrainer extends VocabularyTrainer {
  def train(db: VocabularyDb, examiner: Examiner): Task[Unit] =
    examiner.prepareExam(db).flatMap {
      case None => Task(println("No data to train on"))
      case Some(exam) => runExam(exam)
    }

  private def runExam(exam: Exam): Task[Unit] = {
    exam.nextQuestion(doAsk(exam)).flatMap {
      case Some(feedback) => printFeedback(feedback) *> eventuallyWaitForEnter(feedback) *> clearScreen *> runExam(exam)
      case None => Task.unit
    }
  }

  private def eventuallyWaitForEnter(feedback: Feedback): Task[Unit] =
    if (feedback == Feedback.Postponed) Task.unit else Task {
      print("Press enter to continue")
      StdIn.readLine()
    }.unit

  private def printFeedback(feedback: Feedback): Task[Unit] = Task {
    feedback match {
      case Feedback.Postponed => ()
      case Feedback.Correction(_, score) => score match {
        case Score.Perfect =>
          println(score)

        case _ =>
          println(feedback)
      }
    }
  }

  private def doAsk(exam: Exam)(question: Question): Task[Option[Answer]] =
    for {
      _ <- printQuestion(question, exam.language1, exam.language2)
      input <- readInput
      answer = parseInput(input)
    } yield answer

  private def printQuestion(question: Question, lang1: LanguageName, lang2: LanguageName): Task[Unit] = Task {
    val (knownSide, knownLang) = question.direction match {
      case Direction.LeftToRight => (question.translation.left, lang1)
      case _ => (question.translation.right, lang2)
    }

    val hintStr = question.hint
      .map(hint => s"[hint: ${hint.spelling}] ")
      .getOrElse("")

    print(s"$knownLang[${knownSide.spelling}] --$hintStr--> ")
  }

  private def readInput: Task[String] = Task {
    StdIn.readLine()
  }

  private def clearScreen: Task[Unit] = Task {
    System.out.print("\u001b[H\u001b[2J")
    System.out.flush()
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
      credentialsPath <- GsheetsCfg.sheetId.orDie
      db <- GsheetsVocabularyDb.make(sheetId, new File(credentialsPath))
      trainer = new ConsoleTrainer
      _ <- trainer.train(db, new DefaultExaminer(new LeitnerRepetitionScheme))
    } yield 0).orDie
  }
}
