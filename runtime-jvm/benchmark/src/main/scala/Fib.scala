package org.unisonweb.benchmark

import org.unisonweb.Term
import org.unisonweb.Term.{Name, Term}
import org.unisonweb.compilation._

object Fib extends App {

  implicit class Arithmetic(a: Term) {
    def -(b: Term) = Term.Builtin("-")(a,b)
    def +(b: Term) = Term.Builtin("+")(a,b)
    def *(b: Term) = Term.Builtin("*")(a,b)
  }

   val builtins : String => Computation = {
     case s@"-" => mkBuiltin(s, _ - _)
     case s@"+" => mkBuiltin(s, _ + _)
     case s@"*" => mkBuiltin(s, _ * _)
   }

  def mkBuiltin(name: Name, f: (Double, Double) => Double) = {
    val term = Term.Builtin(name)
    Return {
      new Lambda {
        def arity = 2
        def apply(rec: Lambda, r: R) = { r.boxed = this; 0.0 }

        def apply(rec: Lambda, x0: D, x0b: V, r: R) = { r.boxed = new Lambda {
          def arity = 1

          def apply(rec: Lambda, r: R) = { r.boxed = this; 0.0 }

          def apply(rec: Lambda, x1: D, x1b: V, r: R) = { r.boxed = null; f(x0, x1) }

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
        If0("n", 0.0,
        If0(Var("n") - 1.0, 1.0,
        Var("fib")(Var("n") - 1.0) + Var("fib")(Var("n") - 2.0))))
    )(Var("fib")(N))

  val facTailRec =
    LetRec(
      "fac" -> Lam("n", "acc")(
        If0('n, 1, 'fac.v('n.v - 1, 'n.v * 'acc.v))
      )
    )('fac.v(10, 1))

  List(
    "applyIdentity" -> applyIdentity,
    "identityInLet" -> identityInLet,
    "identityInLet2" -> identityInLet2,
    "identityInLetRec" -> identityInLetRec,
    "identityInLetRec2" -> identityInLetRec2,
    "countFrom" -> countFrom,
    "fib" -> fib,
    "facTailRec" -> facTailRec
  ).foreach {
    case (name, term) =>
      print(s"$name: ")
      val result = normalize(builtins)(term)
      println(result)
  }

  // todo: getting null pointer exception running countFrom
  // paul: suspect that compiling self-calls is busted in how it interacts with general letrec
  // arya: selfcall with null fn is crazeballs


}
