package org.unisonweb.util

import org.unisonweb.compilation2.{U,U0}

object Unboxed {

  abstract class F1[-Env,A,B] {
    def stage(e: Env): (B => Unit) => F1.Compiled[A]
  }

  object F1 {
    def boxedScalaFunction[A,B](f: A => B): F1[Any,A,B] =
      _ => set => (_,a) => { set(f(a)); U0 }

    abstract class Compiled[A] {
      def apply(u: U, a: A): U
    }
  }

  abstract class F2[-Env,A,B,C] {
    def stage(e: Env): (C => Unit) => F2.Compiled[A,B]
  }

  object F2 {
    def boxedScalaFunction[A,B,C](f: (A,B) => C): F2[Any,A,B,C] =
      _ => set => (_,a,_,b) => { set(f(a,b)); U0 }

    abstract class Compiled[A,B] {
      def apply(u: U, a: A, u2: U, b: B): U
    }
  }
}
