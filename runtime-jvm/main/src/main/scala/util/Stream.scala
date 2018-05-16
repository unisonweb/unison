package org.unisonweb
package util

import org.unisonweb.Builtins.FUP_P
import org.unisonweb.util.Stream._
import org.unisonweb.util.Unboxed.{F1, F2, K, Unboxed}

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

  final def scanLeft0[B](u0: U, b0: B)(f: F2[B,A,B]): Stream[B] =
    k => {
      k(u0, b0)
      var u1 = u0
      var b1 = b0
      val cf = f andThen {
        (u, b) =>
          u1 = u
          b1 = b
          k(u1, b1)
      }
      self.stage { (u, a) => cf(u1, b1, u, a) }
    }

  final def scanLeft[C0, C](c0: C0)(f: F2[C,A,C])(implicit C: Extract[C0,C]): Stream[C] =
    scanLeft0(u0 = C.toUnboxed(c0), b0 = C.toBoxed(c0))(f)

  final def sumIntegers(implicit A: A =:= Unboxed[Long]): Long = {
    var total: Long = 0l
    self.stage { (u,_) => total += u }.run()
    total
  }

  final def reduce[B](zero: B)(f2: F2[A,A,A])(implicit A: Extract[B,A]): B = {
    var sumU: U = A.toUnboxed(zero)
    var sumA: A = A.toBoxed(zero)
    val cf = f2 andThen { (u, a) => sumU = u; sumA = a }
    self.stage { (u,a) => cf(sumU, sumA, u, a) }.run()
    A.extract(sumU, sumA)
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

  final def foldLeft0[B,C](u0: U, b0: B)(f: F2[B,A,B])(extract: FUP_P[B,C]): C = {
    var u = u0; var b = b0
    val cf = f andThen { (u2,b2) => u = u2; b = b2 }
    self.stage { (ux,bx) => cf(u, b, ux, bx) }.run()
    extract(u,b)
  }

  /**
    * @param c0 accumulator initial value
    * @param f operator
    * @tparam C0 accumulator Scala type
    * @tparam C stream/boxed type corresponding to Scala type C0
    * @param C converts between C0 and C
    */
  final def foldLeft[C0, C](c0: C0)(f: F2[C,A,C])
                               (implicit C: Extract[C0,C]): C0 =
    foldLeft0(u0 = C.toUnboxed(c0), b0 = C.toBoxed(c0))(f)(C.extract)

  private final def ::(u: U, a: A): Stream[A] =
    k => {
      var left = true
      val cself = self.stage(k)
      () =>
        if (left) {
          left = false
          k(u, a)
        }
        else throw More(cself)
    }

  final def ::[B](b: B)(implicit A: Extract[B,A]): Stream[A] =
    ::(A.toUnboxed(b), A.toBoxed(b))

  final def ++(s: Stream[A]): Stream[A] = k => {
    var cself = self.stage(k)
    val cs = s.stage(k)
    () => {
      try cself()
      catch {
        case Done => throw More(cs)
        case More(m) => cself = m
      }
    }
  }

  final def toSequence0[B](f: FUP_P[A,B]): Sequence[B] = {
    var result = Sequence.empty[B]
    self.stage { (u,a) => result = result :+ f(u,a) }.run()
    result
  }

  final def toSequence[B](implicit A: Extract[B,A]): Sequence[B] =
    toSequence0(A.extract)
}

object Stream {
  @inline def getUnboxed[B](u: U, b: B) = u
  @inline def getBoxed[B](u: U, b: B) = b

  sealed abstract class Extract[Native, Boxed] {
    val extract: FUP_P[Boxed,Native]
    def toBoxed(a: Native): Boxed
    def toUnboxed(a: Native): U
  }
  object Extract {
    implicit val foo: Extract[Param, Value] =
      new Extract[Param, Value] {
        override val extract: FUP_P[Value, B] =
          (u,b) => Value.fromParam(u, b)

        override def toBoxed(a: B): Value = a match {
          case Value.Unboxed(_, typ) => typ
          case v => v.toValue
        }

        override def toUnboxed(a: B): U = a match {
          case Value.Unboxed(n, _) => n
          case _ => U0
        }
      }

    implicit val extractValue: Extract[Value, Value] =
      new Extract[Value, Value] {

        val extract =
          (u,a) => Value(u, a)

        def toBoxed(c: Value): Value = c match {
          case Value.Unboxed(n, typ) => typ
          case v => v
        }

        def toUnboxed(c: Value): U = c match {
          case Value.Unboxed(n, _) => n
          case v => U0
        }
      }

    implicit val extractDoubleValue: Extract[Double, Value] =
      new Extract[Double, Value] {
        val extract: FUP_P[Value, Double] = (u,_) => unboxedToDouble(u)
        def toBoxed(a: Double): Value = UnboxedType.Float
        def toUnboxed(a: Double): U = doubleToUnboxed(a)
      }

    implicit val extractLongValue: Extract[Long, Value] =
      new Extract[Long, Value] {
        val extract: FUP_P[Value, Long] = (u,_) => unboxedToLong(u)
        def toBoxed(a: Long): Value = UnboxedType.Int64
        def toUnboxed(a: Long): U = longToUnboxed(a)
      }

    implicit val extractIntValue: Extract[Int, Value] =
      new Extract[Int, Value] {
        val extract: FUP_P[Value, Int] = (u,_) => unboxedToInt(u)
        def toBoxed(a: Int): Value = UnboxedType.Int64
        def toUnboxed(a: Int): U = intToUnboxed(a)
      }

    implicit val extractDouble: Extract[Double, Unboxed[Double]] =
      new Extract[Double, Unboxed[Double]] {
        val extract = (u,_) => unboxedToDouble(u)
        def toBoxed(c: Double): Unboxed[Double] = null
        def toUnboxed(c: Double): U = doubleToUnboxed(c)
      }

    implicit val extractLong: Extract[Long, Unboxed[Long]] =
      new Extract[Long, Unboxed[Long]] {
        val extract = (u,_) => unboxedToLong(u)
        def toUnboxed(c: Long): U = longToUnboxed(c)
        def toBoxed(c: Long): Unboxed[Long] = null
      }

    implicit val extractInt: Extract[Int, Unboxed[Int]] =
      new Extract[Int, Unboxed[Int]] {
        val extract = (u,_) => unboxedToInt(u)
        def toUnboxed(c: Int): U = intToUnboxed(c)
        def toBoxed(c: Int): Unboxed[Int] = null
      }
  }

  abstract class Step {
    def apply(): Unit

    @inline final def run(): Unit =
      try { while (true) apply() }
      catch {
        case Done =>
        case More(s) => s.run
      }
  }

  case object Done extends Throwable { override def fillInStackTrace = this }
  case class More(s: Step) extends Throwable { override def fillInStackTrace = this }

  final def empty[A]: Stream[A] =
    k => () => throw Done

  final def constant(n: Long): Stream[Unboxed[Long]] =
    k => () => k(n, null)

  private final def constant0[A](u: U, a: A): Stream[A] =
    k => () => k(u, a)

  final def constant[A0,A](a0: A0)(implicit A: Extract[A0,A]): Stream[A] =
    constant0(A.toUnboxed(a0), A.toBoxed(a0))

  /** the arithmetic in here won't work on doubles interpreted as integers */
  final def from(n: Long): Stream[Unboxed[Long]] =
    k => {
      var i = n - 1
      () => { i += 1; k(i,null) }
    }

  final def from(n: Double, by: Double): Stream[Unboxed[Double]] =
    k => {
      var i: Double = n - by
      () => { i += by; k(doubleToUnboxed(i),null) }
    }

  final def fromUnison(n: Long): Stream[UnboxedType] =
    k => {
      var i = n - 1
      () => { i += 1; k(i, UnboxedType.Int64) }
    }

  private final def iterate0[A](u0: U, a0: A)(f: F1[A,A]): Stream[A] = {
    k => {
      var u1 = u0
      var a1 = a0
      val cf = f andThen { (u2, a2) => u1 = u2; a1 = a2 }
      () => {
        val u = u1
        val a = a1
        cf(u1, a1)
        k(u,a)
      }
    }
  }

  final def iterate[A,B](a: A)(f: F1[B,B])(implicit A:Extract[A,B]): Stream[B] =
    iterate0(A.toUnboxed(a), A.toBoxed(a))(f)
}
