package com.github.mlangc.memento.trainer.examiner

import com.github.mlangc.memento.trainer.model.Hint
import com.github.mlangc.memento.trainer.model.UnitIntervalRefinement
import eu.timepit.refined.api.Refined
import scalaz.zio.Ref
import scalaz.zio.UIO
import eu.timepit.refined.auto._
import eu.timepit.refined.refineV
import scalaz.zio.Semaphore

import scala.util.Random

class SpellingHinter private(spelling: String, revealedRef: Ref[Set[Int]], semaphore: Semaphore) {
  def nextHint: UIO[Hint] = semaphore.withPermit {
    for {
      revealed <- revealedRef.get
      hint <- {
        if (revealed.size == spelling.size) {
          UIO.succeed(Hint(spelling, 1.0))
        } else if (revealed.size == spelling.size - 1) {
          revealedRef.set(0.until(spelling.size).toSet) *> UIO.succeed(Hint(spelling, 1.0))
        } else {
          val notRevealed = 0.until(spelling.size).toSet.diff(revealed).toArray
          for {
            ind <- UIO(Random.nextInt(notRevealed.size))
            toReveal = notRevealed(ind)
            revealedNew = revealed + toReveal
            hintSpelling = spelling.zipWithIndex.map { case (c, ind) =>
              if (revealedNew.contains(ind)) c
              else '_'
            }.mkString("")
            revealRatio <- UIO(refineV[UnitIntervalRefinement](revealedNew.size.toDouble / spelling.size).right.get)
            _ <- revealedRef.set(revealedNew)
          } yield Hint(hintSpelling, Refined.unsafeApply(revealRatio))
        }
      }
    } yield hint
  }
}

object SpellingHinter {
  def make(spelling: String): UIO[SpellingHinter] =
    for {
      revealed <- Ref.make(Set.empty[Int])
      semaphore <- Semaphore.make(1)
    } yield new SpellingHinter(spelling, revealed, semaphore)
}
