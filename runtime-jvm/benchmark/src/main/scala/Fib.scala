package org.unisonweb.benchmark

// import org.unisonweb.Render
import org.unisonweb.Term._
// import org.unisonweb.compilation._

object Fib extends App {

  implicit class Arithmetic(a: Term) {
    def -(b: Term) = Builtin("-")(a,b)
    def +(b: Term) = Builtin("+")(a,b)
    def *(b: Term) = Builtin("*")(a,b)
  }

  // val builtins : String => Rt = {
  //   case s@"-" => new Arity2(Builtin(s)) with NF {
  //     def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = x2 - x1
  //   }
  //   case s@"+" => new Arity2(Builtin(s)) with NF {
  //     def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = x2 + x1
  //   }
  //   case s@"*" => new Arity2(Builtin(s)) with NF {
  //     def apply(rec: Rt, x0: D, x0b: Rt, x1: D, x1b: Rt, r: R) = x0 * x1
  //   }
  // }
  //
  // def mkBuiltin(name: Name, f: (Double, Double) => Double) = new Arity2(Builtin(name)) with NF {
  //   def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = f(x1, x2)
  // }

  val N = 15.0

  val countFrom =
    LetRec("countFrom" -> Lam("n")(
      If0('n, 0.0, 'countFrom.v('n.v - 1.0))
    ))('countFrom.v(3.0))

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

// println(normalize(builtins){ println(Render.renderTerm(countFrom)); countFrom })

  Thread.sleep(200)
}
