package org.unisonweb
package util

import Stream._
import Unboxed.{F1,F2,K,Unboxed}

/**
 * Fused stream type based loosely on ideas from Oleg's
 * "Stream Fusion, to Completeness" [1] and also borrowing
 * from FS2's `Segment` type.[2] Stream values are "staged"
 * or compiled to loops that operate directly on mutable
 * values. There is no pattern matching or materialization
 * of intermediate data structures that occurs as part of traversing
 * the stream. Modulo some function call overhead (which the JIT
 * can often inline away), the resulting loop is what one would obtain
 * from writing a monolithic while loop. But we can assemble our
 * loops in a compositional fashion, using our favorite higher order functions!
 *
 * We differ from [1] in that we aren't generating and compiling
 * Scala code, instead we rely on the normal JVM JIT'ing.
 * We differ from [2] in that we use "unboxed" functions and avoid
 * boxing overhead for mapping, filtering, zipWith, etc. (see `Unboxed.scala`).
 *
 * A `Stream[A]` has a single abstract function, `stage`, which
 * takes an "unboxed" callback `K[A]`. Staging returns a `Step` object
 * which must be `.run` to pump the output values of the stream through
 * the provided callback.
 *
 * Public users of `Stream` won't use `stage` directly; instead
 * they construct streams using combinators and consume a stream
 * with a function like `foldLeft`, `toSequence`, `sum`, etc.
 *
 * [1]: https://arxiv.org/abs/1612.06668
 * [2]: https://github.com/functional-streams-for-scala/fs2/blob/series/1.0/core/shared/src/main/scala/fs2/Segment.scala
 */
abstract class Stream[A] { self =>

  def stage(callback: K[A]): Step

  final def map[B](f: F1[A,B]): Stream[B] =
    k => self.stage(f andThen k)

  /** Only emit elements from `this` for which `f` returns a nonzero value. */
  final def filter(f: F1[A,Unboxed[Boolean]]): Stream[A] =
    k => self.stage(Unboxed.choose(f, k, Unboxed.K.noop))

  /** Emit the longest prefix of `this` for which `f` returns nonzero. */
  final def takeWhile(f: F1[A,Unboxed[Boolean]]): Stream[A] =
    k => self.stage(Unboxed.choose[A](f, k, (_,_) => throw Done))

  /** Skip the longest prefix of `this` for which `f` returns nonzero. */
  final def dropWhile(f: F1[A,Unboxed[Boolean]]): Stream[A] =
    k => self.stage(Unboxed.switchWhen0(f, Unboxed.K.noop, k)())

  final def take(n: Long): Stream[A] =
    k => self.stage {
      var rem = n
      (u,a) => if (rem > 0) { rem -= 1; k(u,a) }
               else throw Done
    }

  final def drop(n: Long): Stream[A] =
    k => self.stage {
      var rem = n
      (u,a) => if (rem > 0) rem -= 1
               else k(u,a)
    }

  final def sumIntegers(implicit A: A =:= Unboxed[Long]): Long = {
    var total: Long = 0l
    self.stage { (u,_) => total += u }.run()
    total
  }

  final def sumFloats(implicit A: A =:= Unboxed[Double]): Double = {
    var total: Double = 0
    self.stage { (u,_) => total += unboxedToDouble(u) }.run()
    total
  }

  final def zipWith[B,C](bs: Stream[B])(f: F2[A,B,C]): Stream[C] =
    k => {
      var au = U0; var ab: A = null.asInstanceOf[A]
      val fc = f andThen k
      var askingLeft = true
      val left = self.stage { (u,a) => au = u; ab = a; askingLeft = false }
      val right = bs.stage { (bu,bb) => askingLeft = true; fc(au, ab, bu, bb) }
      () => {
        if (askingLeft) left()
        else right()
      }
    }

  // foldLeft takes `(u0: U, b0: B)` rather than `(c0: C, encode: C => (U,B))`
  // because `encode` would allocate a `Tuple2`
  final def foldLeft[B,C](u0: U, b0: B)(f: F2[B,A,B])(extract: (U,B) => C): C = {
    var u = u0; var b = b0
    val cf = f andThen { (u2,b2) => u = u2; b = b2 }
    self.stage { (ux,bx) => cf(u, b, ux, bx) }.run()
    extract(u,b)
  }

  /**
    * @param c0 accumulator initial value
    * @param f operator
    * @param C converts between C and CB
    * @tparam A0 real type corresponding to stream/boxed type A, eg Long for Unboxed[Long]
    * @tparam C accumulator Scala type
    * @tparam CB stream/boxed type corresponding to Scala type C
    * @return
    */
  final def foldLeft[A0, C, CB](c0: C)(f: F2[CB,A,CB])
                               (implicit C: Extract[C,CB]): C =
    foldLeft(u0 = C.toUnboxed(c0), b0 = C.toBoxed(c0))(f)(C.extract)

  final def ++(s: Stream[A]): Stream[A] = k => {
    var done = false
    val cself = self.stage(k)
    val cs = s.stage(k)
    () => {
      if (done) cs()
      else { try cself() catch { case Done => done = true } }
    }
  }

  final def toSequence0[B](f: (U,A) => B): Sequence[B] = {
    var result = Sequence.empty[B]
    self.stage { (u,a) => result = result :+ f(u,a) }.run()
    result
  }

  final def toSequence[B](implicit A: Extract[B,A]): Sequence[B] =
    toSequence0(A.extract _)
}

object Stream {
  @inline def getUnboxed[B](u: U, b: B) = u
  @inline def getBoxed[B](u: U, b: B) = b

  sealed abstract class Extract[Native, Boxed] {
    def extract(u: U, a: Boxed): Native
    def toBoxed(c: Native): Boxed
    def toUnboxed(c: Native): U
  }
  object Extract {
    implicit val extractValue: Extract[Value, Value] =
      new Extract[Value, Value] {
        def extract(u: U, a: Value): Value = a match {
          case typ: UnboxedType => Value(u, typ)
          case v => v
        }

        def toBoxed(c: Value): Value = c match {
          case Value.Unboxed(n, typ) => typ
          case v => v
        }

        def toUnboxed(c: Value): U = c match {
          case Value.Unboxed(n, _) => n
          case v => U0
        }
      }

    implicit val extractDouble: Extract[Double, Unboxed[Double]] =
      new Extract[Double, Unboxed[Double]] {
        def extract(u: U, a: Unboxed[Double]): Double = unboxedToDouble(u)
        def toBoxed(c: Double): Unboxed[Double] = null
        def toUnboxed(c: Double): U = doubleToUnboxed(c)
      }

    implicit val extractLong: Extract[Long, Unboxed[Long]] =
      new Extract[Long, Unboxed[Long]] {
        def extract(u: U, a: Unboxed[Long]): Long = unboxedToLong(u)
        def toUnboxed(c: Long): U = longToUnboxed(c)
        def toBoxed(c: Long): Unboxed[Long] = null
      }

    implicit val extractInt: Extract[Int, Unboxed[Int]] =
      new Extract[Int, Unboxed[Int]] {
        def extract(u: U, a: Unboxed[Int]): Int = unboxedToInt(u)
        def toUnboxed(c: Int): U = intToUnboxed(c)
        def toBoxed(c: Int): Unboxed[Int] = null
      }
  }

  abstract class Step {
    def apply(): Unit

    @inline final def run(): Unit =
      try { while (true) apply() } catch { case Done => }
  }

  case object Done extends Throwable { override def fillInStackTrace = this }
  // idea: case class More(s: Step) extends Throwable { override def fillInStackTrace = this }

  final def constant(n: Long): Stream[Unboxed[Long]] =
    k => () => k(n, null)

  /** the arithmetic in here won't work on doubles interpreted as integers */
  final def from(n: Long): Stream[Unboxed[Long]] =
    k => {
      var i = n - 1
      () => { i += 1; k(i,null) }
    }

  final def from(n: Double): Stream[Unboxed[Double]] =
    k => {
      var i: Double = n - 1
      () => { i += 1; k(doubleToUnboxed(i),null) }
    }

  final def fromUnison(n: Long): Stream[UnboxedType] =
    k => {
      var i = n - 1
      () => { i += 1; k(i, UnboxedType.Integer) }
    }

  final def fromUnison(n: Double): Stream[UnboxedType] =
    k => {
      var i = n - 1
      () => { i += 1; k(doubleToUnboxed(i), UnboxedType.Float) }
    }

}
