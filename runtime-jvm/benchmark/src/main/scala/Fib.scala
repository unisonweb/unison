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

  def mkBuiltin(name: Name, f: (Double, Double) => Double) = new Computation2(Term.Builtin(name)) {
    def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, r: R) = f(x1, x0)
  }

//  def mkBuiltin(name: Name, f: (Double, Double) => Double) = new ComputationN(2, Term.Builtin(name)) {
//    override def apply(rec: Lambda, xs: Array[Slot], r: R) = f(xs(1).unboxed, xs(0).unboxed)
//  }

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

  val countFrom =
    Lam("n")('n)(3.0)


//    Let(
////      "foo" -> Num(99),
//      "identity" -> Lam("n")(
//        'n
////        If0('n, 50, 'countFrom.v('n.v - 1.0))
//      ),
//    )('identity.v(3.0))

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

  println(normalize(builtins)(countFrom))

  Thread.sleep(200)
}
