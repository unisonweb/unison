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
      def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
        r.boxed = null
        r.unboxed = x2 - x1
      }
    }
    case s@"+" => new Arity2(Builtin(s)) with NF {
      def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
        r.boxed = null
        r.unboxed = x2 + x1
      }
    }
  }

  val N = 15.0

  val fib =
    LetRec(
      "fib" -> Lam("n")(
        If0("n", 0.0,
        If0(Var("n") - 1.0, 1.0,
        Var("fib")(Var("n") - 1.0) + Var("fib")(Var("n") - 2.0))))
    )(Var("fib")(N))

  def fibScala(n: Double): Double =
    if (n == 0.0) 0.0
    else if (n - 1.0 == 0.0) 1.0
    else fibScala(n-1.0) + fibScala(n-2.0)

  val plus = compile(builtins)(Builtin("+"))
  val minus1 = compile(builtins)(Lam("n")(Var("n") - 1.0))
  val minus2 = compile(builtins)(Lam("n")(Var("n") - 2.0))

  val manuallyCompiledFib : Rt = new Arity1(Builtin("fib-manual-compile")) {
    def bind(env: Map[Name,Rt]) = ()
    def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
      if (x1 == 0.0) r.unboxed = 0.0
      else {
        val x12 = eval(rec, minus1, x1, null, r)
        if (r.unboxed == 0.0) r.unboxed = 1.0
        else {
          val r1 = { eval(rec, minus1, x1, null, r); apply(rec, r.unboxed, r.boxed, r); r.unboxed }
          val r2 = { eval(rec, minus2, x1, null, r); apply(rec, r.unboxed, r.boxed, r); r.unboxed }
          plus(null, r1, null, r2, null, r)
        }
      }
    }
    override def isEvaluated = true
  }

  val manuallyCompiledFib2 : Rt = new Arity1(Builtin("fib-manual-compile")) {
    def bind(env: Map[Name,Rt]) = ()
    def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
      if (x1 == 0.0) r.unboxed = 0.0
      else {
        val x12 = eval(rec, minus1, x1, null, r)
        if (r.unboxed == 0.0) r.unboxed = 1.0
        else {
          val r1 = { eval(rec, minus1, x1, null, r); rec(rec, r.unboxed, r.boxed, r); r.unboxed }
          val r2 = { eval(rec, minus2, x1, null, r); rec(rec, r.unboxed, r.boxed, r); r.unboxed }
          plus(null, r1, null, r2, null, r)
        }
      }
    }
    override def isEvaluated = true
  }

  println(normalize(builtins)(fib))
  println(normalize(builtins)(Compiled(manuallyCompiledFib)(Num(N))))
  println(fibScala(N))

  val compiledFib = compile(builtins)(fib)

  QuickProfile.suite(
    QuickProfile.timeit("unison", 0.08) {
      val r = Result()
      compiledFib(null, r)
      (r.unboxed + math.random).toLong
    },
    QuickProfile.timeit("manually-compiled-unison (2)", 0.08) {
      val r = Result()
      manuallyCompiledFib2(manuallyCompiledFib2, N, null, r)
      (r.unboxed + math.random).toLong
    },
    QuickProfile.timeit("manually-compiled-unison", 0.08) {
      val r = Result()
      manuallyCompiledFib(manuallyCompiledFib, N, null, r)
      (r.unboxed + math.random).toLong
    },
    QuickProfile.timeit("scala", 0.08) {
      (fibScala(N) + math.random).toLong
    }
  )

  // println(normalize(builtins)(fib))
  // println(normalize(builtins)(Num(1.0) + Num(4.0)))
}
