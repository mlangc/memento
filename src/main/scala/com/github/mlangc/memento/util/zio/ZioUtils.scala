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
}
