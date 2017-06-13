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

  val N = 30.0
  
  val fib =
    LetRec(
      "fib" -> Lam("n")(
        If0("n", 0.0,
        If0(Var("n") - 1.0, 1.0,
        Var("fib")(Var("n") - 1.0) + Var("fib")(Var("n") - 2.0))))
    )(Var("fib")(N))

  def fibScala(n: Double): Double = 
    if (n == 0) 0 
    else if (n == 1) 1 
    else fibScala(n-1) + fibScala(n-2)

  val compiledFib = compile(builtins)(fib)
  
  QuickProfile.timeit("unison", 0.08) { 
    val r = Result()
    compiledFib(r)
    (r.unboxed + math.random).toLong
  }
  
  QuickProfile.timeit("scala", 0.08) {
    (fibScala(N) + math.random).toLong
  }
  
  // println(normalize(builtins)(fib))
  // println(normalize(builtins)(Num(1.0) + Num(4.0)))
}
