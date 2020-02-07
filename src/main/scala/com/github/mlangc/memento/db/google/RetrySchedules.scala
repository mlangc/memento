package com.github.mlangc.memento.db.google

import java.net.SocketTimeoutException

import com.github.mlangc.slf4zio.api._
import com.google.api.client.http.HttpResponseException
import zio.Schedule
import zio.UIO
import zio.ZIO
import zio.ZSchedule
import zio.clock.Clock
import zio.duration.durationInt

object RetrySchedules extends LoggingSupport{
  def gapiCall(retryStrategy: Throwable => UIO[Boolean] = _ => ZIO.succeed(false)): ZSchedule[Clock, Throwable, Throwable] =
    Schedule.doWhileM(shouldRetry(retryStrategy)) <* (ZSchedule.exponential(125.millis) || ZSchedule.spaced(2.seconds)) <* Schedule.recurs(8)

  private def shouldRetry(retryStrategy: Throwable => UIO[Boolean])(th: Throwable) = th match {
    case e: HttpResponseException if e.getStatusCode == 429 =>
      logger.warnIO("Google API quota exceeded", e).as(true)
    case e: SocketTimeoutException =>
      logger.warnIO("Timeout when interacting with Google APIs", e).as(true)

    case th => retryStrategy(th)
  }
}
