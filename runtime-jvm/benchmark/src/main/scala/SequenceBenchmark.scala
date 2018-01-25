package org.unisonweb.benchmark

import org.unisonweb.util.Sequence

object SequenceBenchmark extends App {

  import QuickProfile.{suite, profile}

  val N = 10000L
  def zero = math.random.floor.toLong

  val seqSum =
    (zero until N).foldLeft(Sequence.empty[Long])(_ :+ _).foldLeft(0L)(_ + _)

  val seqSum2 =
    (zero until N).foldLeft(Sequence.empty[Long])((buf, i) => buf ++ Sequence(i,i,i)).foldLeft(0L)(_ + _)

  // val seqSum3 =
  //  (zero until N).foldLeft(Sequence.empty[Long])((buf, i) => Sequence(i) ++ buf).foldLeft(0L)(_ + _)

  val vecSum =
    (zero until N).foldLeft(Vector.empty[Long])(_ :+ _).foldLeft(0L)(_ + _)

  require(seqSum == vecSum)

  suite(
    {
      val nums = (zero until N).foldLeft(Sequence.empty[Long])(_ :+ _)
      profile("Sequence (++)") { (nums ++ nums).size }
    },
    {
      val nums = Vector.range(zero.toInt, N.toInt)
      profile("Vector (++)") { (nums ++ nums).size.toLong }
    },
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
        val bound = 1024L min N
        while (i < bound) { sum += s(i); i += 1 }
        sum
      }
    },
    {
      val s = (zero until N).foldLeft(Vector.empty[Long])(_ :+ _)
      profile("Vector (apply small)") {
        var i = 0
        var sum = 0L + zero
        val bound = 1024 min N.toInt
        while (i < bound) { sum += s(i); i += 1 }
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
