package org.unisonweb.util

import Stream._

abstract class Stream[A] { self =>
  def stage(k: K[A]): Step

  final def map[B](f: F[A,B]): Stream[B] =
    k => self.stage(f(k))

  final def filter(f: Predicate[A]): Stream[A] =
    k => self stage { (u,a) => if (f(u,a)) k(u,a) }

  final def take(n: Long): Stream[A] =
    k => self stage {
      var rem = n
      (u,a) => if (rem > 0) { rem -= 1; k(u,a) }
               else throw Done
    }

  final def drop(n: Long): Stream[A] =
    k => self stage {
      var rem = n
      (u,a) => if (rem > 0) rem -= 1
               else k(u,a)
    }

  final def takeWhile(f: Predicate[A]): Stream[A] =
    k => self stage { (u,a) =>
      if (f(u,a)) k(u,a)
      else throw Done
    }

  final def dropWhile(f: Predicate[A]): Stream[A] =
    k => self stage {
      var dropping = true
      (u,a) => if (!dropping) k(u,a)
               else if (!f(u,a)) { dropping = false; k(u,a) }
    }

  final def sum(implicit A: A =:= Long): Long = {
    var total = 0L
    val s = self stage { (u,_) => total += u }
    try { while (true) s() } catch { case Done => }
    total
  }

  final def zipWith[B,C](s: Stream[B])(f: F2[A,B,C]): Stream[C] =
    k => {
      var au = 0L; var ab: Ref[A] = null
      val as = self stage { (u,a) => au = u; ab = a }
      val k2 = f(k)
      val bs = s stage { (u,b) => k2(au,ab,u,b) }
      () => { as(); bs() }
    }

  final def evens: Stream[A] = {
    k => self stage {
      var ok = true
      (u,a) => { ok = { if (ok) k(u,a); !ok }}
    }
  }

  final def odds: Stream[A] = {
    k => self stage {
      var ok = false
      (u,a) => { ok = { if (ok) k(u,a); !ok }}
    }
  }

  def ++(s: Stream[A]): Stream[A] = k => {
    var done = false
    val cself = self stage k
    val cs = s stage k
    () => {
      if (done) cs()
      else { try cself() catch { case Done => done = true } }
    }
  }
}

object Stream {
  abstract class Predicate[A] { def apply(u: Long, a: Ref[A]): Boolean }
  abstract class F[A,B] { def apply(k: K[B]): K[A] }
  abstract class F2[A,B,C] { def apply(k: K[C]): K2[A,B] }
  abstract class K[A] { def apply(u: Long, b: Ref[A]): Unit }
  abstract class K2[A,B] { def apply(u1: Long, b1: Ref[A], u2: Long, b2: Ref[B]): Unit }
  abstract class Step { def apply(): Unit }
  case class Ref[A](get: A)

  case object Done extends Throwable { override def fillInStackTrace = this }

  final def constant(n: Long): Stream[Long] = k => () => k(n,null)

  final def range(start: Long, stopExclusive: Long): Stream[Long] =
    k => {
      var i = start - 1
      () => { i += 1; if (i < stopExclusive) k(i,null) else throw Done }
    }

  final def from(n: Long): Stream[Long] =
    k => {
      var i = n - 1
      () => { i += 1; k(i,null) }
    }
}
