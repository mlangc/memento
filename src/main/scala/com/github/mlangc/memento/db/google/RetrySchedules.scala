package com.github.mlangc.memento.db.google

import java.net.SocketTimeoutException

import com.github.mlangc.slf4zio.api._
import com.google.api.client.http.HttpResponseException
import zio.Schedule
import zio.ZIO
import zio.ZSchedule
import zio.clock.Clock
import zio.duration.durationInt

object RetrySchedules extends LoggingSupport{
  val gapiCall: ZSchedule[Clock, Throwable, Throwable] =
    Schedule.doWhileM(shouldRetry) <* ZSchedule.exponential(500.millis) <* Schedule.recurs(6)

  private def shouldRetry(th: Throwable) = th match {
    case e: HttpResponseException if e.getStatusCode == 429 =>
      logger.warnIO("Quota exceeded while loading values", e).as(true)
    case e: SocketTimeoutException =>
      logger.warnIO("Timeout while loading values", e).as(true)

    case _ => ZIO.succeed(false)
  }
}
