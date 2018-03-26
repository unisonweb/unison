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
    },
    test("let7") { implicit T =>
      equal(eval(Let('x1 -> (5.0:Term), 'x2 -> (2.0:Term), 'x3 -> (3.0:Term),
                     'x4 -> (7.0:Term), 'x5 -> (11.0:Term), 'x6 -> (13.0:Term),
                     'x7 -> (17.0:Term))(
        'x1.v * 'x2 * 'x3 * 'x4 * 'x5 * 'x6 * 'x7
      )), 510510.0:Term)
    },
    test("fib") { implicit T =>
      0 to 20 foreach { n =>
        equal(eval(fib(n.toDouble:Term)), scalaFib(n).toDouble:Term)
      }
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

  val fib: Term =
    LetRec('fib ->
      Lam('n)(If('n.v < 2.0, 'n, 'fib.v('n.v - 1) + 'fib.v('n.v - 2))))('fib)

  def scalaFib(n: Int): Int = if (n < 2) n else scalaFib(n - 1) + scalaFib(n - 2)

  implicit class Ops(t0: Term) {
    def +(t1: Term) = '+.b(t0, t1)
    def -(t1: Term) = '-.b(t0, t1)
    def *(t1: Term) = '*.b(t0, t1)
    def /(t1: Term) = '/.b(t0, t1)
    def <(t1: Term) = '<.b(t0, t1)
  }
}
