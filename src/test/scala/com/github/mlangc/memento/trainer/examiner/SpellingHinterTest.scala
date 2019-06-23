package com.github.mlangc.memento.trainer.examiner

import com.github.mlangc.memento.BaseZioTest
import com.github.mlangc.memento.db.model.Spelling
import com.github.mlangc.memento.trainer.model.Hint
import eu.timepit.refined.auto._
import zio.ZIO

class SpellingHinterTest extends BaseZioTest {
  "Test our hinter" - {
    "with a single letter" in {
      assert(takeHints("a", 2) === List(Hint("a", 1.0), Hint("a", 1.0)))
    }

    "with two letters" in {
      val hints = takeHints("he", 3)
      assert(hints.size === 3)
      assert(hints.last.spelling === "he")
      assert(hints.head.spelling.count(_ == '_') === 1)
      genericChecks(hints)
    }

    "with a few letters" in {
      val hints = takeHints("Lausbub", 7)
      assert(hints.size === 7)
      assert(hints.last.spelling === "Lausbub")
      assert(hints.head.spelling.count(_ == '_') === 6)
      genericChecks(hints)
    }
  }

  private def genericChecks(hints: List[Hint]): Unit = {
    assert(hints == hints.sortBy(_.revealed.value))
    assert(hints == hints.sortBy(_.spelling.count(_ != '_')))

    hints.foreach { hint =>
      val numChars = hint.spelling.size
      val numUnderscores = hint.spelling.count(_ == '_')

      val expectedRevealed = if (numChars == 0) 1.0 else {
        (numChars - numUnderscores).toDouble/numChars
      }

      assert(expectedRevealed === hint.revealed.value)
    }
  }

  private def takeHints(spelling: Spelling, n: Int): List[Hint] = {
    unsafeRun {
      for {
        hinter <- SpellingHinter.make(spelling)
        res <- ZIO.collectAll(Iterable.fill(n)(hinter.nextHint))
      } yield res
    }
  }
}
