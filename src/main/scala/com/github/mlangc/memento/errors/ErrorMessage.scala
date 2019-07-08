package com.github.mlangc.memento.errors

class ErrorMessage(message: String, cause: Throwable = null) extends RuntimeException(message, cause) {
  def this(cause: Throwable) = this(cause.getMessage, cause)
}
