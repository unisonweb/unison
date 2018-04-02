package org.unisonweb.util

import org.unisonweb.compilation2.U

abstract class Unboxed1[-Env,A,B] {
  def stage(e: Env): (B => Unit) => Unboxed1.Compiled[A,B]
}

object Unboxed1 {
  abstract class Compiled[A,B] {
    def apply(u: U, a: A): U
  }
}

abstract class Unboxed2[-Env,A,B,C] {
  def stage(e: Env): (C => Unit) => Unboxed2.Compiled[A,B,C]
}

object Unboxed2 {

  abstract class Compiled[A,B,C] {
    def apply(u: U, a: A, u2: U, b: B): U
  }
}
