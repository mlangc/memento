package com.github.mlangc.memento.trainer.model

import java.time.Instant

import cats.instances.list._
import cats.kernel.Order
import cats.syntax.foldable._
import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.db.model.Vocabulary
import com.github.mlangc.memento.i18n.MotivatorMessages
import com.github.mlangc.memento.trainer.model.Question.Motivator
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative

case class Question(translation: Translation,
                    direction: Direction,
                    timesAskedBefore: Int Refined NonNegative,
                    lastAsked: Option[Instant] = None,
                    hint: Option[Hint] = None,
                    motivators: List[Motivator] = Nil) {
  def rightAnswer: Vocabulary = direction match {
    case Direction.LeftToRight => translation.right
    case Direction.RightToLeft => translation.left
  }

  def toCard: Card = Card(translation, direction)
}

object Question {

  trait Motivator {
    def text(messages: MotivatorMessages): String
  }

  def create(translation: Translation,
             direction: Direction,
             checks: List[Check],
             hint: Option[Hint] = None,
             motivators: List[Motivator] = Nil): Question = {
    val correspondingCard = Card(translation, direction)
    val relatedChecks = checks.filter(correspondingCard.correspondsTo)
    val order = Order.reverse(Order.by[Check, Instant](_.timestamp)(Order.fromComparable[Instant]))
    val lastAsked = relatedChecks.minimumOption(order).map(_.timestamp)
    val timesAskedBefore = Refined.unsafeApply[Int, NonNegative](relatedChecks.size)
    Question(translation, direction, timesAskedBefore, lastAsked, hint, motivators)
  }
}
