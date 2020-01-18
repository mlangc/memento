package com.github.mlangc.memento.db.cache

trait Cachable[A] {
  def toBytes(a: A): Array[Byte]
  def fromBytes(bytes: Array[Byte]): Either[IllegalArgumentException, A]
}
