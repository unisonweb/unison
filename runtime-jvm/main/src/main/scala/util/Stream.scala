package org.unisonweb
package util

import Stream._
import compilation2.{U,U0}
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
  final def filter(f: F1[A,U]): Stream[A] =
    k => self.stage(Unboxed.choose(f, k, Unboxed.K.noop))

  /** Emit the longest prefix of `this` for which `f` returns nonzero. */
  final def takeWhile(f: F1[A,U]): Stream[A] =
    k => self.stage(Unboxed.choose[A](f, k, (_,_) => throw Done))

  /** Skip the longest prefix of `this` for which `f` returns nonzero. */
  final def dropWhile(f: F1[A,U]): Stream[A] =
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

  final def sum(implicit A: A =:= Unboxed[U]): U = {
    var total = U0
    self.stage { (u,_) => total += u }.run()
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

  def foldLeft[B,C](u0: U, b0: B)(f: F2[B,A,B])(extract: (U,B) => C): C = {
    var u = U0; var b = b0
    val cf = f andThen { (u2,b2) => u = u2; b = b2 }
    self.stage { (u2,a) => cf(u, b, u2, a) }.run()
    extract(u,b)
  }

  def box[T](f: U => T)(implicit A: A =:= Unboxed[T]): Stream[T] =
    map(new Unboxed.F1[A,T] {
      def apply[x] = ktx => (u,_,u2,x) => ktx(U0,f(u),u2,x)
    })

  def ++(s: Stream[A]): Stream[A] = k => {
    var done = false
    val cself = self.stage(k)
    val cs = s.stage(k)
    () => {
      if (done) cs()
      else { try cself() catch { case Done => done = true } }
    }
  }

  def toSequence[B](f: (U,A) => B): Sequence[B] = {
    var result = Sequence.empty[B]
    self.stage { (u,a) => result = result :+ f(u,a) }.run()
    result
  }
}

object Stream {

  abstract class Step {
    def apply(): Unit

    @inline final def run(): Unit =
      try { while (true) apply() } catch { case Done => }
  }

  case object Done extends Throwable { override def fillInStackTrace = this }
  // idea: case class More(s: Step) extends Throwable { override def fillInStackTrace = this }

  final def constant(n: U): Stream[Unboxed[U]] =
    k => () => k(n, null)

  final def from(n: U): Stream[Unboxed[U]] =
    k => {
      var i = n - 1
      () => { i += 1; k(i,null) }
    }
}
