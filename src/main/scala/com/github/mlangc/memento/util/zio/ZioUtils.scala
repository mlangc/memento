package com.github.mlangc.memento.util.zio

import zio.UIO
import zio.ZIO

object ZioUtils {
  def cached[R, E, A](zio: ZIO[R, E, A])(get: UIO[Option[A]], set: A => UIO[Unit]): ZIO[R, E, A] =
    get.some.orElse(zio.tap(a => set(a)))
}
