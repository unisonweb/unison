package org.unisonweb.benchmark

import org.unisonweb.util.Sequence

object SequenceBenchmark extends App {

  import QuickProfile.{suite, profile}

  val N = 100000L
  def zero = math.random.floor.toLong

  val seqSum =
    (zero until N).foldLeft(Sequence.empty[Long])(_ :+ _).foldLeft(0L)(_ + _)

  val seqSum2 =
    (zero until N).foldLeft(Sequence.empty[Long])(_ :+ _).foldLeft(0L)(_ + _)

  val vecSum =
    (zero until N).foldLeft(Vector.empty[Long])(_ :+ _).foldLeft(0L)(_ + _)

  require(seqSum == vecSum)

  suite(
    profile("Sequence (snoc)") {
      (zero until N).foldLeft(Sequence.empty[Long])(_ :+ _).size
    },
    profile("Vector (snoc)") {
      (zero until N).foldLeft(Vector.empty[Long])(_ :+ _).size.toLong
    },
    {
      val s = (zero until N).foldLeft(Sequence.empty[Long])(_ :+ _)
      profile("Sequence (apply small)") {
        var i = 0L
        var sum = 0L + zero
        while (i < 1024L) { sum += s(i); i += 1 }
        sum
      }
    },
    {
      val s = (zero until N).foldLeft(Vector.empty[Long])(_ :+ _)
      profile("Vector (apply small)") {
        var i = 0
        var sum = 0L + zero
        while (i < 1024) { sum += s(i); i += 1 }
        sum
      }
    },
    {
      val s = (zero until N).foldLeft(Sequence.empty[Long])(_ :+ _)
      profile("Sequence (apply)") {
        var i = 0L
        var sum = 0L + zero
        while (i < N) { sum += s(i); i += 1 }
        sum
      }
    },
    {
      val s = (zero until N).foldLeft(Vector.empty[Long])(_ :+ _)
      profile("Vector (apply)") {
        var i = 0
        var sum = 0L + zero
        while (i < N) { sum += s(i); i += 1 }
        sum
      }
    }
  )
}
