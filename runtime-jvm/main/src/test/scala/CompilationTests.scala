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
    test("sum4(1,2,3,4)") { implicit T =>
      equal(eval(sum4(1,10,100,1000)), (1+10+100+1000):Term)
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
    test("fib-pretty-print") { implicit T =>
      import org.unisonweb.util.PrettyPrint
      note("pretty-printed fib implementation", includeAlways = true)
      note(PrettyPrint.prettyTerm(fib).render(40), includeAlways = true)
      note("pretty-printed fib implementation in ANF", includeAlways = true)
      note(PrettyPrint.prettyTerm(Term.ANF(fib)).render(40), includeAlways = true)
      ok
    },
    test("fib-ANF") { implicit T =>
      val fibANF = Term.ANF(fib)
      0 to 20 foreach { n =>
        equal1(eval(fibANF(n:Term)), scalaFib(n):Term)
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
    test("triangle4arg") { implicit T =>
      10 to 50 foreach { n =>
        equal1(eval(triangle4arg(n:Term, zero, zero, zero)), (0 to n).sum:Term)
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
    test("shadow2") { implicit T =>
      val fib = LetRec('fib -> Lam('fib)('fib.v + 1))('fib.v(41))
      val fibNested = LetRec('fib -> (1: Term))(fib)
      equal(eval(fibNested), 42:Term)
    },
    test("let rec example 1") { implicit T =>
      val ex = LetRec(
        'x -> 1,
        'y -> 10,
        'y2 -> 100,
        'z -> 1000
      )('x.v + 'y + 'y2 + 'z)
      equal(eval(ex), 1111: Term)
    },
    test("let rec example 2") { implicit T =>
      val ex = LetRec(
        'x -> 1,
        'y -> 10,
        'y2 -> 100,
        'z -> 1000
      )('x.v + 'y2 + 'z)
      equal(eval(ex), 1101: Term)
    },
    test("let rec example 3") { implicit T =>
      val ex = LetRec(
        'x -> 1,
        'y -> 10,
        'y2 -> 100,
        'z -> 1000
      )('y.v + 'y2 + 'z)
      equal(eval(ex), 1110: Term)
    },
    test("let rec example 4") { implicit T =>
      val ex = LetRec(
        'x -> 1,
        'y -> 10,
        'y2 -> 'x.v * 100,
        'z -> 1000
      )('y2.v)
      equal(eval(ex), 100: Term)
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
      },
      test("ex2 (underapplication)") { implicit T =>
        val t: Term =
          Let('x -> Sequence(1,2,3),
              'fn -> Sequence.take(2))(Sequence.size('fn.v('x)))
        equal(eval(t), 2: Term)
      }
    ),
    suite("text") (
      test("examples") { implicit T =>
        equal1(eval(Text.concatenate("abc", "123")), "abc123": Term)
        equal1(eval(Text.concatenate(Text.empty, "123")), "123": Term)
        equal1(eval(Text.concatenate("123", Text.empty)), "123": Term)
        equal1(eval(Text.drop(3, "abc123")), "123": Term)
        equal1(eval(Text.take(3, "abc123")), "abc": Term)
        equal1(eval(Text.equal("abc", "abc")), true: Term)
        equal1(eval(Text.lt("Alice", "Bob")), true: Term)
        equal1(eval(Text.lteq("Runar", "Runarorama")), true: Term)
        equal1(eval(Text.lt("Arya", "Arya-orama")), true: Term)
        equal1(eval(Text.gt("Bob", "Alice")), true: Term)
        equal1(eval(Text.gteq("Runarorama", "Runar")), true: Term)
        equal1(eval(Text.gteq("Arya-orama", "Arya")), true: Term)
        ok
      }
    ),
    suite("pattern")(
      test("literal") { implicit T =>
        /* let x = 42; case 10 of 10 -> x + 1 */
        val v: Term = 43
        val c = MatchCase(LiteralU(10, UnboxedType.Integer), 'x.v + 1)
        val p = Let('x -> (42:Term))(Match(10)(c))
        equal(eval(p), v)
      },
      test("pattern guard") { implicit T =>
        /* let x = 42
           case 10 of
             10 | true -> x + 1
        */
        val v: Term = 43
        val c = MatchCase(LiteralU(10, UnboxedType.Integer),
                          Some(true:Term), 'x.v + 1)
        val p = Let('x -> (42:Term))(Match(10)(c))
        equal(eval(p), v)
      },
      test("wildcard") { implicit T =>
        /* let x = 42; case 10 of
           10 | false -> x + 1;
           y -> y + 4
          should be 14
        */
        val v: Term = 14
        val c1 = MatchCase(LiteralU(10, UnboxedType.Integer),
                           Some(false:Term), 'x.v + 1)
        val c2 = MatchCase(Wildcard, ABT.Abs('y, 'y.v + 4))
        val p = Let('x -> (42:Term))(Match(10)(c1, c2))
        equal(eval(p), v)
      },
      test("wildcard0") { implicit T =>
        /* case 10 of y -> y + 4 */
        val v: Term = 14
        val c = MatchCase(Wildcard, ABT.Abs('y, 'y.v + 4))
        val p = Match(10)(c)
        equal(eval(p), v)
      },
      test("uncaptured") { implicit T =>
        /* let x = 42; case 10 of 10 | false -> x + 1; _ -> x + 2
           should return 44
        */
        val v: Term = 44
        val c1 = MatchCase(LiteralU(10, UnboxedType.Integer),
                           Some(false:Term), 'x.v + 1)
        val c2 = MatchCase(Uncaptured, 'x.v + 2)
        val p = Let('x -> (42:Term))(Match(10)(c1, c2))
        equal(eval(p), v)
      },
      test("shadowing") { implicit T =>
        /* let x = 42; case 10 of 10 | false -> x+1; x -> x+4 */
        val v: Term = 14
        val c1 = MatchCase(LiteralU(10, UnboxedType.Integer),
                           Some(false:Term), 'x.v + 1)
        val c2 = MatchCase(Wildcard, ABT.Abs('x, 'x.v + 4))
        val p = Let('x -> (42:Term))(Match(10)(c1, c2))
        equal(eval(p), v)
      },
      test("data pattern") { implicit T =>
        /* let x = 42; case (2,4) of (x,y) -> x+y; x -> x + 4 */
        val v: Term = 6
        val c1 = MatchCase(Pattern.Tuple(Wildcard, Wildcard),
                           ABT.Abs('x, ABT.Abs('y, 'x.v + 'y.v)))
        val c2 = MatchCase(Wildcard, ABT.Abs('x, 'x.v + 4))
        val p = Let('x -> (42:Term))(Match(intTupleTerm(2, 4))(c1, c2))
        equal(eval(p), v)
      },
      test("big non-nested data pattern") { implicit T =>
        /* let x = 42; case (1,10,100) of (a,b,c) -> a+b+c */
        val v: Term = 111
        val c1 = MatchCase(
          Pattern.Tuple(Wildcard, Wildcard, Wildcard),
          ABT.AbsChain('a, 'b, 'c)('a.v + 'b + 'c))
        val p = Let('x -> (42:Term))(Match(intTupleTerm(1, 10, 100))(c1))
        equal(eval(p), v)
      },
      test("bigger non-nested data pattern") { implicit T =>
        /* let x = 42; case (1,10,100,1000) of (a,b,c,d) -> a+b+c+d */
        val v: Term = 1111
        val c1 = MatchCase(
          Pattern.Tuple(Wildcard, Wildcard, Wildcard, Wildcard),
          ABT.AbsChain('a, 'b, 'c, 'd)('a.v + 'b + 'c + 'd))
        val p = Let('x -> (42:Term))(Match(intTupleTerm(1, 10, 100, 1000))(c1))
        equal(eval(p), v)
      },
      test("nested data patterns") { implicit T =>
        /* let x = 42; case ((3,4),(5,6)) of ((x,y),(_,z)) -> x+y+z; x -> x + 4 */
        val v: Term = 13
        val c1 =
          MatchCase(Pattern.Tuple(
            Pattern.Tuple(Wildcard, Wildcard),
            Pattern.Tuple(Uncaptured, Wildcard)),
                    ABT.AbsChain('x, 'y, 'z)('x.v + 'y + 'z))
        val c2 = MatchCase(Wildcard, ABT.Abs('x, 'x.v + 4))
        val p = Let('x -> (42:Term))(
          Match(Terms.tupleTerm(intTupleV(3, 4), intTupleV(5, 6)))(c1, c2))
        equal(eval(p), v)
      },
      test("fall through data pattern") { implicit T =>
        /* let x = 42; case (2,4) of (x,y) -> x+y; x -> x + 4 */
        val v: Term = 6
        val c1 = MatchCase(Pattern.Left(Wildcard), ABT.Abs('x, 'x))
        val c2 = MatchCase(Pattern.Right(Wildcard), ABT.Abs('x, 'x))
        val p = Match(intRightTerm(6))(c1, c2)
        equal(eval(p), v)
      },
      test("fall through data pattern 2") { implicit T =>
        /* let x = 42; case (2,4) of (x,y) -> x+y; x -> x + 4 */
        val v: Term = 8
        val c1 = MatchCase(Pattern.Left(Wildcard), ABT.Abs('x, 'x.v + 1))
        val c2 = MatchCase(Pattern.Right(Wildcard), ABT.Abs('x, 'x.v + 2))
        val p = Match(intRightTerm(6))(c1, c2)
        equal(eval(p), v)
      },
      test("patterns that read the stack array") { implicit T =>
        /* let x = 42; case (2,4) of (x,y) -> x+y; x -> x + 4 */
        val c1 = MatchCase(Pattern.Left(Wildcard), ABT.Abs('x, 'x.v + 'a))
        val c2 = MatchCase(Pattern.Right(Wildcard), ABT.Abs('x, 'x.v + 'a))
        val p =
          Let('a -> 1, 'b -> 10)(Match(intRightTerm(6))(c1, c2))
        equal[Term](eval(p), 7)
      },
      test("as pattern") { implicit T =>
        /* case 3 of x@(y) -> x + y */
        val v: Term = 6
        val c =
          MatchCase(Pattern.As(Pattern.Wildcard),
                    ABT.AbsChain('x, 'y)('x.v + 'y))
        val p = Match(3)(c)
        equal(eval(p), v)
      },
      test("as-as-literal") { implicit T =>
        /* case 3 of x@(y@(3)) -> x + y */
        val v: Term = 6
        val c =
          MatchCase(Pattern.As(Pattern.As(Pattern.LiteralU(3, UnboxedType.Integer))),
                    ABT.AbsChain('x, 'y)('x.v + 'y))
        val p = Match(3)(c)
        equal(eval(p), v)
      },
      test("as-guard-literal 1") { implicit T =>
        /*  case 3 of
              x@(y@(3)) | x + y > 4 -> x + y
         */
        val v: Term = 6
        val c =
          MatchCase(
            Pattern.As(
              Pattern.As(
                Pattern.LiteralU(3, UnboxedType.Integer)
              )), Some[Term](ABT.AbsChain('x, 'y)('x.v + 'y > 4)), ABT.AbsChain('x, 'y)('x.v + 'y))
        val p = Match(3)(c)
        equal(eval(p), v)
      },
      test("as-guard-literal 2") { implicit T =>
        /*  case 1 of
              x@(y@(3)) | x + y > 4 -> x + y
              _ -> 2
         */
        val v: Term = 2
        val c =
          MatchCase(
            Pattern.As(
              Pattern.As(
                Pattern.LiteralU(3, UnboxedType.Integer)
              )), Some[Term](ABT.AbsChain('x, 'y)('x.v + 'y > 4)), ABT.AbsChain('x, 'y)('x.v + 'y))
        val c2 = MatchCase[Term](Pattern.Uncaptured, 2)
        val p = Match(1)(c, c2)
        equal(eval(p), v)
      },
    ),
    test("nested applies") { implicit T =>
      val p = Apply(Apply(triangle, 10), 0)
      equal(eval(p), eval(triangle(10, 0)))
    },
    //suite("algebraic-effects")(
    //  test("ex1") { implicit T =>
    //    /*
    //      let
    //        state : s -> <State s> a -> a
    //        state s <a> = a
    //        state s <get -> k> = handle (state s) (k s)
    //        state _ <put s -> k> = handle (state s) (k ())

    //        handle (state 0)
    //          x = State.get + 1
    //          y = State.set (x + 1)
    //          State.get + 11
    //     */
    //    val p = LetRec(
    //      ('state, Lam('s, 'action) {
    //        Match('action)(
    //          // state s <a> = a
    //          MatchCase(Pattern.EffectPure(Pattern.Wildcard),
    //                    ABT.Abs('a, 'a)),

    //          // state s <get -> k> = handle (state s) (k s)
    //          MatchCase(Pattern.EffectBind(Id.Builtin("State"),
    //                                       ConstructorId(0),
    //                                       Nil),
    //                    ABT.Abs('k, Handle('state.v('s))('k.v('s)))),

    //          // state _ <put s -> k> = handle (state s) (k ())
    //          MatchCase(Pattern.EffectBind(Id.Builtin("State"),
    //                                       ConstructorId(1),
    //                                       List(Pattern.Wildcard)),
    //                    ABT.AbsChain('s, 'k)(Handle('state.v('s))('k.v(-1))))
    //        )
    //      })
    //    ) {
    //      Handle('state.v(0)) {
    //        Let(
    //          ('x, Request(Id.Builtin("State"), ConstructorId(0), Nil) + 1),
    //          ('y, Request(Id.Builtin("State"), ConstructorId(1), List('x.v + 1)))
    //        )(Request(Id.Builtin("State"), ConstructorId(0), Nil) + 11)
    //      }
    //    }
    //    equal[Term](eval(p), 13)
    //  }
    //)
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

  val sum4: Term = Lam('a,'b,'c,'d)('a.v + 'b + 'c + 'd)

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

  val triangle4arg =
    LetRec('triangle ->
             Lam('n, 'hahaha, 'hehehe, 'acc)(
               If('n.v,
                  'triangle.v('n.v - 1, 'hahaha, 'hehehe, 'acc.v + 'n),
                  'acc.v))
    )('triangle)

  val odd =
    LetRec(
      'even -> Lam('n)(If('n.v > zero, 'odd.v ('n.v - 1), one)),
      'odd-> Lam('n)(If('n.v > zero, 'even.v ('n.v - 1), zero))
    )('odd)

  def tupleTerm(xs: Value*): Term =
    Term.Compiled(tupleV(xs :_*))

  def tupleV(xs: Value*): Value =
    Value.Data(Id.Builtin("Tuple"), ConstructorId(0), xs.toArray)

  def intTupleTerm(xs: Int*): Term =
    Term.Compiled(intTupleV(xs: _*))

  def intRightTerm(i: Int): Term =
    Term.Compiled(intRightV(i))

  def intRightV(i: Int): Value =
    Value.Data(Id.Builtin("Either"), ConstructorId(1), Array(intValue(i)))
  def intTupleV(xs: Int*): Value =
    tupleV(xs.map(intValue): _*)

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

  object Text {
    import Builtins._

    val empty = termFor(Text_empty)
    val take = termFor(Text_take)
    val drop = termFor(Text_drop)
    val concatenate = termFor(Text_concatenate)
    val size = termFor(Text_size)
    val equal = termFor(Text_eq)
    val lt = termFor(Text_lt)
    val gt = termFor(Text_gt)
    val lteq = termFor(Text_lteq)
    val gteq = termFor(Text_gteq)
  }
}
