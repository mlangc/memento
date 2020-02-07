package com.github.mlangc.memento.util.zio

case class RefinementError(value: Any, message: String) extends IllegalArgumentException(s"Error refining '$value': $message'")

