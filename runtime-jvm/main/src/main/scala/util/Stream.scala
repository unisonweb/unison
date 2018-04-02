package org.unisonweb
package util

import Stream._
import compilation2.{U,U0}

abstract class Stream[Env,A] { self =>

  def stage(e: Env): K[A] => Step

  def covaryEnv[E2<:Env]: Stream[E2,A] = this.asInstanceOf[Stream[E2,A]]

  final def map[B](f: Unboxed1[Env,A,B]): Stream[Env,B] =
    env => k => {
      var b : B = null.asInstanceOf[B]
      val fc = f.stage(env) (br => b = br)
      self.stage(env) { (u,a) => k(fc(u,a), b) }
    }

  /** Only emit elements from `this` for which `f` returns a nonzero value. */
  final def filter(f: Unboxed1[Env,A,U]): Stream[Env,A] =
    env => k => {
      val fc = f.stage(env) { br => }
      self.stage(env) { (u,a) => if (fc(u,a) != U0) k(u,a) }
    }

  /** Emit the longest prefix of `this` for which `f` returns nonzero. */
  final def takeWhile(f: Unboxed1[Env,A,U]): Stream[Env,A] =
    env => k => self.stage(env) {
      val fc = f.stage(env) { br => }
      (u,a) => if (fc(u,a) != U0) k(u,a)
               else throw Done
    }

  /** Skip the longest prefix of `this` for which `f` returns nonzero. */
  final def dropWhile(f: Unboxed1[Env,A,U]): Stream[Env,A] =
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

  final def sum(env: Env)(implicit A: A =:= U): U = {
    var total = U0
    val s = self.stage(env) { (u,_) => total += u }
    try { while (true) s() } catch { case Done => }
    total
  }

  final def zipWith[B,C](bs: Stream[Env,B])(f: Unboxed2[Env,A,B,C]): Stream[Env,C] =
    env => k => {
      var au = U0; var ab: A = null.asInstanceOf[A]; var cb: C = null.asInstanceOf[C]
      val fc = f.stage(env) { c => cb = c }
      val left = self.stage(env) { (u,a) => au = u; ab = a }
      val right = bs.stage(env) { (bu,bb) => k(fc(au,ab,bu,bb), cb) }
      () => { left(); right(); ab = null.asInstanceOf[A]; cb = null.asInstanceOf[C] }
    }

  def ++(s: Stream[Env,A]): Stream[Env,A] = env => k => {
    var done = false
    val cself = self.stage(env)(k)
    val cs = s.stage(env)(k)
    () => {
      if (done) cs()
      else { try cself() catch { case Done => done = true } }
    }
  }
}

object Stream {

  abstract class K[A] { def apply(u: U, b: A): Unit }
  abstract class Step { def apply(): Unit }

  case object Done extends Throwable { override def fillInStackTrace = this }

  final def constant(n: U): Stream[Any,U] =
    _ => k => () => k(n, null.asInstanceOf[U])

  final def from(n: U): Stream[Any,U] =
    _ => k => {
      var i = n - 1
      () => { i += 1; k(i,null.asInstanceOf[U]) }
    }
}
