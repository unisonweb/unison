package org.unisonweb.benchmark

import org.unisonweb.Term._
import org.unisonweb.Runtime._

object Fib extends App {

  implicit class Arithmetic(a: Term) {
    def -(b: Term) = Builtin("-")(a,b)
    def +(b: Term) = Builtin("+")(a,b)
  }

  val builtins : String => Rt = {
    case s@"-" => new Arity2(Builtin(s)) with NF {
      def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
        r.boxed = null
        r.unboxed = x2 - x1
      }
    }
    case s@"+" => new Arity2(Builtin(s)) with NF {
      def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
        r.boxed = null
        r.unboxed = x2 + x1
      }
    }
  }

  val fib =
    LetRec("fib" ->
      Lam("n")(
        If0("n", 0.0,
        If0(Var("n") - 1.0, 1.0,
            Var("fib")(Var("n") - 1.0) + Var("fib")(Var("n") - 2.0)))))(Var("fib")(0.0))

  println(normalize(builtins)(fib))
}
