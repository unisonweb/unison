package org.unisonweb.benchmark

import org.unisonweb.{Render1, Term}
import org.unisonweb.Term._
import org.unisonweb.compilation._

object Fib extends App {

  implicit class Arithmetic(a: Term) {
    def -(b: Term) = Term.Builtin("-")(a,b)
    def +(b: Term) = Term.Builtin("+")(a,b)
    def *(b: Term) = Term.Builtin("*")(a,b)
  }

   val builtins : Name => Computation = ({
     case s@"-" => mkBuiltin(s, _ - _)
     case s@"+" => mkBuiltin(s, _ + _)
     case s@"*" => mkBuiltin(s, _ * _)
     case s => sys.error("unknown builtin: " + s)
   }: String => Computation).compose[Name](_.toString)

  def mkBuiltin(name: Name, f: (Double, Double) => Double) = {
    val term = Term.Builtin(name)
    Return {
      new Lambda {
        def arity = 2
        def apply(rec: Lambda, r: R) = { r.boxed = this; 0.0 }

        def apply(rec: Lambda, x0: D, x0b: V, r: R) = { r.boxed = new Lambda {
          def arity = 1

          def apply(rec: Lambda, r: R) = { r.boxed = this; 0.0 }

          def apply(rec: Lambda, x1: D, x1b: V, r: R) = { r.boxed = null; f(x1, x0) }

          def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, r: R) = ???
          def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, r: R) = ???
          def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, x3: D, x3b: V, r: R) = ???
          def apply(rec: Lambda, xs: Array[Slot], r: R) = ???
          def decompile = term(decompileSlot(x0, x0b))
        }; 0.0 }

        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, r: R) = { r.boxed = null; f(x1, x0) }

        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, r: R) = ???
        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, x3: D, x3b: V, r: R) = ???
        def apply(rec: Lambda, xs: Array[Slot], r: R) = ???
        def decompile = term
      }
    }(term)
  }

  /** Compile and evaluate a term, the return result back as a term. */
  def normalize(builtins: Name => Computation)(e: Term): Term = {
    val rt = compile(builtins)(e)
    val r = Result()
    decompileSlot(try rt(null, r) catch { case e: TC => loop(e,r) }, r.boxed)
  }
  def decompileSlot(unboxed: D, boxed: Value): Term =
    if (boxed eq null) Term.Num(unboxed)
    else boxed.decompile

  import Term._
  val N = 15.0

  val identity = Lam("n")('n)
  val applyIdentity = Lam("n")('n)(3.0)
  val identityInLet = Let("identity" -> identity)('identity.v(3.0))
  val identityInLet2 = Let("foo" -> 99, "identity" -> identity)('identity.v(3.0))
  val identityInLetRec = LetRec("identity" -> identity)('identity.v(3.0))
  val identityInLetRec2 = LetRec("foo" -> 99, "identity" -> identity)('identity.v(3.0))
  val countFrom =
    LetRec(
      "foo" -> Num(99),
      "countFrom" -> Lam("n")(If0('n, 50, 'countFrom.v('n.v - 1.0)))
    )('countFrom.v(3.0))

  val fib =
    LetRec(
      "fib" -> Lam("n")(
        If0('n, 0.0,
        If0(Var("n") - 1.0, 1.0,
        Var("fib")(Var("n") - 1.0) + Var("fib")(Var("n") - 2.0))))
    )(Var("fib")(N))

  // let fac n acc = if0(n) then 1 else fac(n-1, n*acc)
  val facTailRec =
    LetRec(
      "fac" -> Lam("n", "acc")(
        If0('n, 'acc, 'fac.v('n.v - 1, 'n.v * 'acc.v))
      )
    )('fac.v(10, 1))

  //    fac n = n * (fac (n-1))
  val facRec = LetRec("fac" -> Lam("n")(If0('n, 1, 'n.v * 'fac.v('n.v - 1))))('fac.v(10))

  val first = Lam('a, 'b)('a)
  val second = Lam('a, 'b)('b)

  val lambdaFvs = Let1("fv1", 77)(Let1("fv2", 20)(Lam('a, 'b)('fv1.v + 'a.v)(1, 2)))

  List(
//    "applyIdentity" -> applyIdentity -> 3.0,
//    "identityInLet" -> identityInLet -> 3.0,
//    "identityInLet2" -> identityInLet2 -> 3.0,
//    "identityInLetRec" -> identityInLetRec -> 3.0,
//    "identityInLetRec2" -> identityInLetRec2 -> 3.0,
//    "countFrom" -> countFrom -> 99.0,
//    "fib" -> fib -> 610.0,
//    "lambdaFvs" -> lambdaFvs -> 78.0,
//    "first" -> first(3, 4) -> 3.0,
//    "second" -> second(3, 4) -> 4.0,
//    "partiallyAppliedFirst" -> first(3)(4) -> 3.0,
//    "partiallyAppliedSecond" -> second(3)(4) -> 4.0,
//    "facTailRec" -> facTailRec -> 3628800.0,
    "facRec" -> facRec -> 3628800.0
  ).foreach {
    case ((name, term), d) =>
      print(f"$name%20s:\t")
      val result = normalize(builtins)(term)
      println(f"${Render1.render(result)}%10s\texpected: $d%10.1f")
  }

  // todo: getting null pointer exception running countFrom
  // paul: suspect that compiling self-calls is busted in how it interacts with general letrec
  // arya: selfcall with null fn is crazeballs


}
