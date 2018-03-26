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
    },
    test("partially apply") { implicit T =>
      equal(eval(const(zero)), Lam('y)(zero))
    },
    test("partially apply builtin") { implicit T =>
      equal(eval(onePlus), onePlus)
      equal(eval(ap(onePlus, one)), eval(onePlusOne))
    },
    test("let") { implicit T =>
      equal(eval(Let('x -> one)(one + 'x)), eval(onePlusOne))
    },
    test("let2") { implicit T =>
      equal(eval(Let('x -> one, 'y -> (2.0:Term))(
        'x.v + 'y
      )), 3.0:Term)
    },
    test("let3") { implicit T =>
      equal(eval(Let('x -> one, 'y -> (10.0:Term), 'z -> (100.0:Term))(
        'x.v + 'y + 'z
      )), 111.0:Term)
    }
  )
}

object Terms {
  val zero: Term = U0
  val one: Term = 1.0

  val id: Term = Lam('x)('x)

  val const: Term = Lam('x,'y)('x)

  val onePlusOne: Term = one + one

  val onePlus: Term = '+.b(one)

  val ap: Term = Lam('f,'x)('f.v('x))

  implicit class Ops(t0: Term) {
    def +(t1: Term) = '+.b(t0, t1)
    def -(t1: Term) = '-.b(t0, t1)
    def *(t1: Term) = '*.b(t0, t1)
    def /(t1: Term) = '/.b(t0, t1)
  }
}
