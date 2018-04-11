package org.unisonweb

import Term._
import compilation._
import Pattern._

object CompilationTests {
  import EasyTest._
  import Terms._

  def eval(t: Term): Term =
    normalize(Builtins.builtins)(t)

  val tests = suite("compilation")(
    test("zero") { implicit T =>
      equal(eval(zero), zero)
    },
    test("id") { implicit T =>
      equal(eval(id(zero)), zero)
    },
    test("1 + 1 = 2") { implicit T =>
      equal(eval(onePlusOne), 2:Term)
    },
    test("1 + 2 = 3") { implicit T =>
      equal(eval((1:Term) + (2:Term)), 3:Term)
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
      equal(eval(Let('x -> one, 'y -> (2:Term))(
        'x.v + 'y
      )), 3:Term)
    },
    test("let3") { implicit T =>
      equal(eval(Let('x -> one, 'y -> (10:Term), 'z -> (100:Term))(
        'x.v + 'y + 'z
      )), 111:Term)
    },
    test("let7") { implicit T =>
      equal(eval(Let('x1 -> (5:Term), 'x2 -> (2:Term), 'x3 -> (3:Term),
                     'x4 -> (7:Term), 'x5 -> (11:Term), 'x6 -> (13:Term),
                     'x7 -> (17:Term))(
        'x1.v * 'x2 * 'x3 * 'x4 * 'x5 * 'x6 * 'x7
      )), 510510:Term)
    },
    test("if") { implicit T =>
      equal1(eval(If(one, one, zero + one + one)), one)
      equal1(eval(If(one - one, one, zero + one + one)), 2:Term)
      equal1(eval(If(one > zero, one, zero + one + one)), one)
      equal1(eval(If(one < zero, one, zero + one + one)), 2:Term)
      ok
    },

    test("fib") { implicit T =>
      0 to 20 foreach { n =>
        equal1(eval(fib(n:Term)), scalaFib(n):Term)
      }
      ok
    },
    test("==") { implicit T =>
      equal1(eval(one unisonEquals one), eval(true))
      equal1(eval(one unisonEquals onePlusOne), eval(false))
      ok
    },
    test("triangle") { implicit T =>
      0 to 50 foreach { n =>
        equal1(eval(triangle(n:Term, zero)), (0 to n).sum:Term)
      }
      ok
    },
    test("evenOdd") { implicit T =>
      0 to 50 foreach { n =>
        equal1(eval(odd(n:Term)), n % 2 :Term)
      }
      ok
    },
    test("nested invokeDynamic") { implicit T =>
      val nestedInvokeDynamic =
        Let(
          'id0 -> id(id),
          'id1 -> 'id0.v('id0),
          'id2 -> 'id1.v('id1)
        ) {
          'id2.v(10)
        }
      equal(eval(nestedInvokeDynamic), 10:Term)
    },
    test("overapply") { implicit T =>
      equal(eval(id(id, id, 10:Term)), 10:Term)
    },
    test("shadow") { implicit T =>
      equal(eval(LetRec('fib -> Lam('fib)('fib.v + 1))('fib.v(41))), 42:Term)
    },
    test("mutual non-tail recursion") { implicit T =>
      0 to 20 foreach { n =>
        equal1(eval(fibPrime(n:Term)), scalaFib(n):Term)
      }
      ok
    },
    suite("sequence")(
      test("take") { implicit T =>
        1 to 20 foreach { n =>
          val xs = replicate(intIn(0,n))(int).map(x => x: Term)
          val mySeq = Sequence(xs:_*)
          val theirSeq = Sequence(xs.take(n):_*)
          equal1(eval(Sequence.take(n, mySeq)), eval(theirSeq))
        }
        ok
      },
      test("ex1") { implicit T =>
        equal(eval(Sequence.size(Sequence(1,2,3))), 3: Term)
      }
    ),
    suite("pattern")(
      test("literal") { implicit T =>
        val v: Term = 43
        val c = MatchCase(LiteralU(10, UnboxedType.Integer), 'x.v + 1)
        val p = Let('x -> (42:Term))(Match(10)(c))
        equal(eval(p), v)
      },
      test("pattern guard") { implicit T =>
        val v: Term = 43
        val c = MatchCase(LiteralU(10, UnboxedType.Integer),
                  Some(true:Term), 'x.v + 1)
        val p = Let('x -> (42:Term))(Match(10)(c))
        equal(eval(p), v)
      },
      test("wildcard") { implicit T =>
        val v: Term = 14
        val c1 = MatchCase(LiteralU(10, UnboxedType.Integer),
                   Some(false:Term), 'x.v + 1)
        val c2 = MatchCase(Wildcard, ABT.Abs('y, 'y.v + 4))
        val p = Let('x -> (42:Term))(Match(10)(c1, c2))
        equal(eval(p), v)
      },
      test("uncaptured") { implicit T =>
        val v: Term = 44
        val c1 = MatchCase(LiteralU(10, UnboxedType.Integer),
                   Some(false:Term), 'x.v + 1)
        val c2 = MatchCase(Uncaptured, 'x.v + 2)
        val p = Let('x -> (42:Term))(Match(10)(c1, c2))
        equal(eval(p), v)
      },
      test("shadowing") { implicit T =>
        val v: Term = 14
        val c1 = MatchCase(LiteralU(10, UnboxedType.Integer),
                   Some(false:Term), 'x.v + 1)
        val c2 = MatchCase(Wildcard, ABT.Abs('x, 'x.v + 4))
        val p = Let('x -> (42:Term))(Match(10)(c1, c2))
        equal(eval(p), v)
      },
      test("data pattern") { implicit T =>
        val v: Term = 6
        val c1 = MatchCase(Pattern.Pair(Wildcard, Wildcard),
                  ABT.Abs('x, ABT.Abs('y, 'x.v + 'y.v)))
        val c2 = MatchCase(Wildcard, ABT.Abs('x, 'x.v + 4))
        val p = Let('x -> (42:Term))(Match(intPair(2, 4))(c1, c2))
        equal(eval(p), v)
      },
      test("nested data patterns") { implicit T =>
        val v: Term = 13
        val c1 =
          MatchCase(Pattern.Pair(
                      Pattern.Pair(Wildcard, Wildcard),
                      Pattern.Pair(Uncaptured, Wildcard)),
            ABT.AbsChain('x, 'y, 'z)('x.v + 'y + 'z))
        val c2 = MatchCase(Wildcard, ABT.Abs('x, 'x.v + 4))
        val p = Let('x -> (42:Term))(
          Match(Terms.pair(intPairV(3, 4), intPairV(5,6)))(c1, c2))
        equal(eval(p), v)
      },
    )
  )
}

object Terms {
  val zero: Term = U0
  val one: Term = 1

  val id: Term = Lam('x)('x)

  val const: Term = Lam('x,'y)('x)

  val onePlusOne: Term = one + one

  val onePlus: Term = Builtins.termFor(Builtins.Integer_add)(one)

  val ap: Term = Lam('f,'x)('f.v('x))

  val fib: Term =
    LetRec('fib ->
             Lam('n)(If('n.v < 2, 'n, 'fib.v('n.v - 1) + 'fib.v('n.v - 2))))('fib)

  val fibPrime: Term =
    LetRec(
      'fib -> Lam('n)(If('n.v < 2, 'n, 'fib2.v('n.v - 1) + 'fib2.v('n.v - 2))),
      'fib2 -> Lam('n)(If('n.v < 2, 'n, 'fib.v('n.v - 1) + 'fib.v('n.v - 2)))
    )('fib)

  def scalaFib(n: Int): Int =
    if (n < 2) n else scalaFib(n - 1) + scalaFib(n - 2)

  val triangle =
    LetRec('triangle ->
             Lam('n, 'acc)(
               If('n.v,
                  'triangle.v('n.v - 1, 'acc.v + 'n),
                  'acc.v))
    )('triangle)

  val odd =
    LetRec(
      'even -> Lam('n)(If('n.v > zero, 'odd.v ('n.v - 1), one)),
      'odd-> Lam('n)(If('n.v > zero, 'even.v ('n.v - 1), zero))
    )('odd)

  def pair(x: Value, y: Value): Term =
    Term.Compiled(pairV(x, y))

  def pairV(x: Value, y: Value): Value =
    Value.Data(Hash(null), ConstructorId(0), Array(x, y))

  def intPair(x: Int, y: Int): Term =
    Term.Compiled(intPairV(x, y))

  def intPairV(x: Int, y: Int): Value =
    pairV(intValue(x), intValue(y))

  def intValue(x: Int): Value = Value.Unboxed(x.toLong, UnboxedType.Integer)

  implicit class Ops(t0: Term) {
    def +(t1: Term) = Builtins.termFor(Builtins.Integer_add)(t0, t1)
    def -(t1: Term) = Builtins.termFor(Builtins.Integer_sub)(t0, t1)
    def *(t1: Term) = Builtins.termFor(Builtins.Integer_mul)(t0, t1)
    def unisonEquals(t1: Term) =
      Builtins.termFor(Builtins.Integer_eq)(t0, t1)
    def <(t1: Term) = Builtins.termFor(Builtins.Integer_lt)(t0, t1)
    def >(t1: Term) = Builtins.termFor(Builtins.Integer_gt)(t0, t1)
  }

  object Sequence {
    import Builtins._

    def apply(terms: Term*): Term =
      terms.foldLeft(empty)((seq, v) => snoc(seq, v))

    val empty = termFor(Sequence_empty)
    val cons = termFor(Sequence_cons)
    val snoc = termFor(Sequence_snoc)
    val take = termFor(Sequence_take)
    val size = termFor(Sequence_size)
  }
}
