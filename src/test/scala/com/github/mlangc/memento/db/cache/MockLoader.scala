package com.github.mlangc.memento.db.cache

import cats.Monoid
import cats.instances.map._
import com.github.mlangc.memento.db.cache.MockLoader.Stats
import com.github.mlangc.memento.db.cache.MockLoader.Stats.statsMonoid
import zio.IO
import zio.Ref
import cats.syntax.monoid._
import zio.UIO

class MockLoader[A, B, E] private(f: A => IO[E, B], journal: Ref[Map[A, Stats]]) {
  def load(a: A): IO[E, B] =
    journal.update(_ |+| Map(a -> Stats(1, 0))) *>
      f(a) <*
      journal.update(_ |+| Map(a -> Stats(0, 1)))

  def stats(a: A): UIO[Stats] =
    journal.get.map(_.getOrElse(a, Stats.Empty))
}

object MockLoader {
  def make[A, B, E](f: A => IO[E, B]): UIO[MockLoader[A, B, E]] =
    Ref.make(Map.empty[A, Stats]).map(new MockLoader[A, B, E](f, _))

  case class Stats(attempted: Int, successful: Int)

  object Stats {
    val Empty: Stats = Stats(0, 0)

    implicit val statsMonoid: Monoid[Stats] = new Monoid[Stats] {
      def empty: Stats = Empty
      def combine(x: Stats, y: Stats): Stats = Stats(x.attempted + y.attempted, x.successful + y.successful)
    }
  }

}
