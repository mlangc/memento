package com.github.mlangc.memento.db.google

import java.net.SocketTimeoutException

import com.github.mlangc.slf4zio.api._
import com.google.api.client.http.HttpResponseException
import zio.UIO
import zio.ZIO
import zio.Schedule
import zio.clock.Clock
import zio.duration.durationInt

object RetrySchedules extends LoggingSupport{
  def gapiCall(retryStrategy: Throwable => UIO[Boolean] = _ => ZIO.succeed(false)): Schedule[Clock, Throwable, Throwable] =
    Schedule.doWhileM(shouldRetry(retryStrategy)) <* (Schedule.exponential(125.millis) || Schedule.spaced(2.seconds)) <* Schedule.recurs(8)

  private def shouldRetry(retryStrategy: Throwable => UIO[Boolean])(th: Throwable) = th match {
    case e: HttpResponseException if e.getStatusCode == 429 =>
      logger.warnIO("Google API quota exceeded", e).as(true)
    case e: SocketTimeoutException =>
      logger.warnIO("Timeout when interacting with Google APIs", e).as(true)

    case th => retryStrategy(th)
  }
}
