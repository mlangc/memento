package com.github.mlangc.memento.trainer.repetition.leitner

import java.util.concurrent.TimeUnit

import com.github.mlangc.memento.db.google.sheets.GsheetsTestHelpers
import com.github.mlangc.memento.trainer.model.TrainingData
import com.github.mlangc.memento.zenvs.ZLayers
import eu.timepit.refined.auto._
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Fork
import org.openjdk.jmh.annotations.Measurement
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.Warmup
import zio.Runtime
import zio.RIO
import zio.UIO

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 1)
@State(Scope.Benchmark)
@Fork(1)
@Measurement(iterations = 3)
class LeitnerRepetitionSchemeBenchmark {
  private var data: TrainingData = _

  @Setup
  def test(): Unit = zRun {
    for {
      db <- GsheetsTestHelpers.initDb("1aTxcJPle729tUt69b6ObSFSqT248coTGghrfqQTtTvo")
      data <- db.load
      _ <- UIO(this.data = TrainingData.convert(data))
    } yield ()
  }

  @Benchmark
  def initScheme = zRun {
    val scheme = new LeitnerRepetitionScheme()

    for {
      scheme <- scheme.implement(data).someOrFail(new NoSuchElementException)
      question <- scheme.next
    } yield question
  }

  private def zRun[A](io: RIO[ZLayers.AppEnv, A]): A =
    Runtime.default.unsafeRun(io.provideLayer(ZLayers.live))
}
