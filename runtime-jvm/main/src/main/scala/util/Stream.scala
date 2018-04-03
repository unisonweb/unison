package org.unisonweb
package util

import Stream._
import compilation2.{U, U0}
import Unboxed.{F1, F2}

abstract class Stream[Env,A] { self =>

  def stage(e: Env): K[A] => Step

  def covaryEnv[E2<:Env]: Stream[E2,A] = this.asInstanceOf[Stream[E2,A]]

  final def map[B](f: F1[Env,A,B]): Stream[Env,B] =
    env => k => {
      var b : B = null.asInstanceOf[B]
      val fc = f.stage(env) (br => b = br)
      self.stage(env) { (u,a) => k(fc(u,a), b) }
    }

  /** Only emit elements from `this` for which `f` returns a nonzero value. */
  final def filter(f: F1[Env,A,U]): Stream[Env,A] =
    env => k => {
      val fc = f.stage(env) { br => }
      self.stage(env) { (u,a) => if (fc(u,a) != U0) k(u,a) }
    }

  /** Emit the longest prefix of `this` for which `f` returns nonzero. */
  final def takeWhile(f: F1[Env,A,U]): Stream[Env,A] =
    env => k => self.stage(env) {
      val fc = f.stage(env) { br => }
      (u,a) => if (fc(u,a) != U0) k(u,a)
               else throw Done
    }

  /** Skip the longest prefix of `this` for which `f` returns nonzero. */
  final def dropWhile(f: F1[Env,A,U]): Stream[Env,A] =
    env => k => self.stage(env) {
      val fc = f.stage(env) { br => }
      var dropping = true
      (u,a) => if (!dropping) k(u,a)
               else if (fc(u,a) == U0) { dropping = false; k(u,a) }
    }


  final def take(n: Long): Stream[Env,A] =
    env => k => self.stage(env) {
      var rem = n
      (u,a) => if (rem > 0) { rem -= 1; k(u,a) }
               else throw Done
    }

  final def drop(n: Long): Stream[Env,A] =
    env => k => self.stage(env) {
      var rem = n
      (u,a) => if (rem > 0) rem -= 1
               else k(u,a)
    }

  final def sum(env: Env)(implicit A: A =:= Unboxed[U]): U = {
    var total = U0
    self.stage(env) { (u,_) => total += u }.run()
    total
  }

  final def zipWith[B,C](bs: Stream[Env,B])(f: F2[Env,A,B,C]): Stream[Env,C] =
    env => k => {
      var au = U0; var ab: A = null.asInstanceOf[A]; var cb: C = null.asInstanceOf[C]
      val fc = f.stage(env) { c => cb = c }
      val left = self.stage(env) { (u,a) => au = u; ab = a }
      val right = bs.stage(env) { (bu,bb) => k(fc(au,ab,bu,bb), cb) }
      () => { left(); right(); ab = null.asInstanceOf[A]; cb = null.asInstanceOf[C] }
    }

  def foldLeft[B,C](env: Env, u0: U, b0: B)(f: F2[Env,B,A,B])(extract: (U,B) => C): C = {
    var (u, b) = (U0, b0)
    val cf = f.stage(env) { b2 => b = b2 }
    self.stage(env) { (u2,a) => u = cf(u, b, u2, a) }.run()
    extract(u,b)
  }

  def box[T](f: U => T)(implicit A: A =:= Unboxed[T]): Stream[Env,T] =
    map(env => set => (u,_) => { set(f(u)); U0 })

  def ++(s: Stream[Env,A]): Stream[Env,A] = env => k => {
    var done = false
    val cself = self.stage(env)(k)
    val cs = s.stage(env)(k)
    () => {
      if (done) cs()
      else { try cself() catch { case Done => done = true } }
    }
  }

  def toSequence[B](env: Env)(f: (U,A) => B): Sequence[B] = {
    var result = Sequence.empty[B]
    self.stage(env) { (u, a) => result = result :+ f(u,a) }.run()
    result
  }
}

object Stream {

  abstract class K[A] { def apply(u: U, b: A): Unit }
  abstract class Step {
    def apply(): Unit

    @inline final def run(): Unit =
      try { while (true) apply() } catch { case Done => }
  }

  /**
   * Marker type with no instances. A `Stream[E,Unboxed[T]]` implies
   * the stream emits only `null` for the boxed portion of its output and that
   * there exists a `U => T` for extracting a `T` from the unboxed portion of
   * its output.
   */
  sealed abstract class Unboxed[T]

  case object Done extends Throwable { override def fillInStackTrace = this }

  final def constant(n: U): Stream[Any,Unboxed[U]] =
    _ => k => () => k(n, null)

  final def from(n: U): Stream[Any,Unboxed[U]] =
    _ => k => {
      var i = n - 1
      () => { i += 1; k(i,null) }
    }
}
