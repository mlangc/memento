package com.github.mlangc.memento.util.zio

import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.Validate
import zio.IO
import zio.UIO
import zio.ZIO
import eu.timepit.refined.refineV


object ZioUtils {
  def cached[R, E, A](zio: ZIO[R, E, A])(get: UIO[Option[A]], set: A => UIO[Unit]): ZIO[R, E, A] =
    get.some.orElse(zio.tap(a => set(a)))

  def refineIO[P] = new RefineIOPartiallyApplied[P]

  final class RefineIOPartiallyApplied[P] {
    def apply[A](a: A)(implicit validate: Validate[A, P]): IO[RefinementError, A Refined P] =
      ZIO.fromEither(refineV[P](a)).mapError(msg => RefinementError(a, msg))
  }

  def accumulateWhile[R, E, A, B](a0: A, b0: B)(combine: (B, B) => B)(h: A => ZIO[R, E, (Option[A], B)]): ZIO[R, E, B] =
    h(a0).flatMap {
      case (None, b) => ZIO.succeed(combine(b0, b))
      case (Some(a), b) => accumulateWhile(a, combine(b0, b))(combine)(h)
    }
}
