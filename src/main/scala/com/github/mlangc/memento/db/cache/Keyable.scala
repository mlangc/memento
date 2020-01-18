package com.github.mlangc.memento.db.cache

import cats.Contravariant

trait Keyable[A] {
  def toKey(a: A): CacheKey
}

object Keyable {
  implicit val keyableContravariant: Contravariant[Keyable] = new Contravariant[Keyable] {
    def contramap[A, B](fa: Keyable[A])(f: B => A): Keyable[B] = b => fa.toKey(f(b))
  }
}
