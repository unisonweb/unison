package org.unisonweb
package util

import Stream._
import compilation2.{Ref => _, _}
// import Env.Env

object Env {
  type Env = (Array[U], Array[B], StackPtr, Result)
}

abstract class Unboxed1[-Env,A,B] {
  def stage(e: Env): (B => Unit) => Unboxed1.Compiled[A,B]
}

object Unboxed1 {
  def fromLambda(f: Value.Lambda): Unboxed1[Env.Env,Any,Any] =
    env => set => {
      val (stackU, stackB, top, r) = env
      (u,a) => {
        val out = evalLam(f, r, top, stackU, U0, u, stackB, null, a.asInstanceOf[Param])
        set(r.boxed)
        out
      }
    }

  abstract class Compiled[A,B] {
    def apply(u: U, a: Ref[A]): U
  }
}

abstract class Stream[Env,A] { self =>

  def stage(e: Env): K[A] => Step

  def covaryEnv[E2<:Env]: Stream[E2,A] = this.asInstanceOf[Stream[E2,A]]

  final def map(f: Unboxed1[Env,A,B]): Stream[Env,B] =
    env => k => {
      var b : Ref[B] = null
      val ufn = f.stage(env) (br => b = Ref(br))
      self.stage(env) { (u,a) => k(ufn(u,a), b) }
    }

  //final def filter(f: Predicate[A]): Stream[A] =
  //  k => self stage { (u,a) => if (f(u,a)) k(u,a) }

  //final def take(n: Long): Stream[A] =
  //  k => self stage {
  //    var rem = n
  //    (u,a) => if (rem > 0) { rem -= 1; k(u,a) }
  //             else throw Done
  //  }

  //final def drop(n: Long): Stream[A] =
  //  k => self stage {
  //    var rem = n
  //    (u,a) => if (rem > 0) rem -= 1
  //             else k(u,a)
  //  }

  //final def takeWhile(f: Predicate[A]): Stream[A] =
  //  k => self stage { (u,a) =>
  //    if (f(u,a)) k(u,a)
  //    else throw Done
  //  }

  //final def dropWhile(f: Predicate[A]): Stream[A] =
  //  k => self stage {
  //    var dropping = true
  //    (u,a) => if (!dropping) k(u,a)
  //             else if (!f(u,a)) { dropping = false; k(u,a) }
  //  }

  //final def sum(implicit A: A =:= Long): Long = {
  //  var total = 0L
  //  val s = self stage { (u,_) => total += u }
  //  try { while (true) s() } catch { case Done => }
  //  total
  //}

  //final def zipWith[B,C](s: Stream[B])(f: F2[A,B,C]): Stream[C] =
  //  k => {
  //    var au = 0L; var ab: Ref[A] = null
  //    val as = self stage { (u,a) => au = u; ab = a }
  //    val k2 = f(k)
  //    val bs = s stage { (u,b) => k2(au,ab,u,b) }
  //    () => { as(); bs() }
  //  }

  //final def evens: Stream[A] = {
  //  k => self stage {
  //    var ok = true
  //    (u,a) => { ok = { if (ok) k(u,a); !ok }}
  //  }
  //}

  //final def odds: Stream[A] = {
  //  k => self stage {
  //    var ok = false
  //    (u,a) => { ok = { if (ok) k(u,a); !ok }}
  //  }
  //}

  //def ++(s: Stream[A]): Stream[A] = k => {
  //  var done = false
  //  val cself = self stage k
  //  val cs = s stage k
  //  () => {
  //    if (done) cs()
  //    else { try cself() catch { case Done => done = true } }
  //  }
  //}
}

object Stream {

  abstract class K[A] { def apply(u: U, b: Ref[A]): Unit }
  abstract class K2[A,B] { def apply(u1: U, b1: Ref[A], u2: U, b2: Ref[B]): Unit }
  abstract class Step { def apply(): Unit }
  case class Ref[B](get: B)

  case object Done extends Throwable { override def fillInStackTrace = this }

  final def constant(n: U): Stream[Any,U] =
    _ => k => () => k(n, null)

  final def from(n: U): Stream[Any,U] =
    _ => k => {
      var i = n - 1
      () => { i += 1; k(i,null) }
    }
}
