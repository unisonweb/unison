package org.unisonweb

import Term._
import compilation2._

object CompilationTests {
  import EasyTest._
  import Terms._

  def eval(t: Term): Term =
    normalize(Lib2.builtins)(t)

  val tests = suite("compilation")(
    test("zero") { implicit T =>
      equal(eval(zero), zero)
    },
    test("id") { implicit T =>
      equal(eval(id(zero)), zero)
    },
    test("1 + 1 = 2") { implicit T =>
      equal(eval(onePlusOne), 2.0:Term)
    }
  )
}

object Terms {
  val zero: Term = U0
  val one: Term = 1.0

  val id: Term = Lam('x)('x)

  val onePlusOne: Term = one + one

  implicit class Ops(t0: Term) {
    def +(t1: Term) = '+.b(t0, t1)
    def -(t1: Term) = '-.b(t0, t1)
    def *(t1: Term) = '*.b(t0, t1)
    def /(t1: Term) = '/.b(t0, t1)
  }
}
