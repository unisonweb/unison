package org.unisonweb.util

import org.unisonweb.compilation2.U

object Unboxed {
  abstract class F1[-Env,A,B] {
    def stage(e: Env): (B => Unit) => F1.Compiled[A,B]
  }

  object F1 {
    abstract class Compiled[A,B] {
      def apply(u: U, a: A): U
    }
  }

  abstract class F2[-Env,A,B,C] {
    def stage(e: Env): (C => Unit) => F2.Compiled[A,B,C]
  }

  object F2 {
    abstract class Compiled[A,B,C] {
      def apply(u: U, a: A, u2: U, b: B): U
    }
  }
}
