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
        r.unboxed = x2 - x1
      }
    }
    case s@"+" => new Arity2(Builtin(s)) with NF {
      def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
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
        // val x12 = eval(null, minus1, x1, null, r)
        if (x1 == 1.0) r.unboxed = 1.0
        // if (r.unboxed == 0.0) r.unboxed = 1.0
        else {
          val r1 = { apply(null, x1 - 1.0, null, r); r.unboxed }
          val r2 = { apply(null, x1 - 2.0, null, r); r.unboxed }
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
        if (x1 == 1.0) r.unboxed = 1.0
        else {
          val r1 = { rec(rec, x1 - 1.0, null, r); r.unboxed }
          val r2 = { rec(rec, x1 - 2.0, null, r); r.unboxed }
          plus(null, r1, null, r2, null, r)
        }
      }
    }
    override def isEvaluated = true
  }

  // observation - function call overhead is bad for stuff like addition
  // fib (n - 1)
  // has to pass n to decrement fn, then take result and pass that to fib
  // n has to be copied to arg
  // basically, too much copying around
  // wonder if there's some way to decrease that overhead?
  // n - 1

  case class R2(var get: Rt2)

  abstract class Rt2 {
    def apply(rec: Rt2, r: R2): Double
    def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double
    def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double
  }
  object Rt2 {
    case class TC(fn: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2) extends Throwable { override def fillInStackTrace = this }

    @annotation.tailrec
    def loop(tc: TC, r: R2): Double =
      try tc.fn(tc.fn, tc.x1, tc.x1b, tc.x2, tc.x2b, r)
      catch { case tc: TC => loop(tc, r) }

    //def eval(rec: Rt2, e: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double =
    //  try e(rec, x1, x1b, x2, x2b, r)
    //  catch { case tc0: TC => loop(tc0, r) }

    //def eval(rec: Rt2, e: Rt2, x1: D, x1b: Rt2, r: R2): Double =
    //  try e(rec, x1, x1b, r)
    //  catch { case tc0: TC => loop(tc0, r) }

    def eval(rec: Rt2, e: Rt2, r: R2): Double =
      try e(rec, r)
      catch { case tc0: TC => loop(tc0, r) }

    val x1 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double = ???
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double =
        { if (!(x1b eq null)) r.get = x1b; x1 }
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double =
        { if (!(x1b eq null)) r.get = x1b; x1 }
    }
    val rec = new Rt2 {
      def apply(rec: Rt2, r: R2): Double = { r.get = rec; 0.0 }
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double = { r.get = rec; 0.0 }
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double = { r.get = rec; 0.0 }
    }
    def if0(cond: Rt2, if0: Rt2, ifNot0: Rt2): Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double =
        if ({ try cond(rec,r) catch { case e: TC => loop(e, r) }} == 0.0) if0(rec, r)
        else ifNot0(rec, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double =
        if ({ try cond(rec,x1,x1b,r) catch { case e: TC => loop(e, r) }} == 0.0) if0(rec, r)
        else ifNot0(rec, x1, x1b, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double =
        if ({ try cond(rec,x1,x1b,x2,x2b,r) catch { case e: TC => loop(e, r) }} == 0.0) if0(rec, r)
        else ifNot0(rec, x1, x1b, x2, x2b, r)
    }
    def if1(cond: Rt2, if1: Rt2, ifNot1: Rt2): Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double =
        if ({ try cond(rec,r) catch { case e: TC => loop(e, r) }} == 1.0) if1(rec, r)
        else ifNot1(rec, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double =
        if ({ try cond(rec,x1,x1b,r) catch { case e: TC => loop(e, r) }} == 1.0) if1(rec, r)
        else ifNot1(rec, x1, x1b, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double =
        if ({ try cond(rec,x1,x1b,x2,x2b,r) catch { case e: TC => loop(e, r) }} == 1.0) if1(rec, r)
        else ifNot1(rec, x1, x1b, x2, x2b, r)
    }
    val primPlus: Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2) = ???
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2) = ???
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2) = x1 + x2
    }
    def plus(x: Rt2, y: Rt2): Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double =
        { try x(rec, r) catch { case e: TC => loop(e,r) }} +
        { try y(rec, r) catch { case e: TC => loop(e,r) }}
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double =
        { try x(rec,x1,x1b,r) catch { case e: TC => loop(e,r) }} +
        { try y(rec,x1,x1b,r) catch { case e: TC => loop(e,r) }}
        // throw new TC(primPlus,
        //  { try x(rec,x1,x1b,r) catch { case e: TC => loop(e,r) }}, null,
        //  { try y(rec,x1,x1b,r) catch { case e: TC => loop(e,r) }}, null)
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double =
        { try x(rec,x1,x1b,x2,x2b,r) catch { case e: TC => loop(e,r) }} +
        { try y(rec,x1,x1b,x2,x2b,r) catch { case e: TC => loop(e,r) }}
    }
    def minus(x: Rt2, y: Rt2): Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double =
        { try x(rec, r) catch { case e: TC => loop(e,r) }} -
        { try y(rec, r) catch { case e: TC => loop(e,r) }}
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double =
        { try x(rec,x1,x1b,r) catch { case e: TC => loop(e,r) }} -
        { try y(rec,x1,x1b,r) catch { case e: TC => loop(e,r) }}
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double =
        { try x(rec,x1,x1b,x2,x2b,r) catch { case e: TC => loop(e,r) }} -
        { try y(rec,x1,x1b,x2,x2b,r) catch { case e: TC => loop(e,r) }}
    }
    def num(n: Double): Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double = n
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double = n
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double = n
    }
    def apRec(a: Rt2): Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2) =
        rec(rec, { try a(rec,r) catch { case e: TC => loop(e,r) }}, r.get, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2) =
        rec(rec, { try a(rec,x1,x1b,r) catch { case e: TC => loop(e,r) }}, r.get, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2) =
        rec(rec, { try a(rec,x1,x1b,x2,x2b,r) catch { case e: TC => loop(e,r) }}, r.get, r)
    }
    def ap(fn: Rt2, a: Rt2) = new Rt2 {
      def apply(rec: Rt2, r: R2) =
        fn(fn, { try a(rec,r) catch { case e: TC => loop(e,r) }}, r.get, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2) =
        fn(fn, { try a(rec,x1,x1b,r) catch { case e: TC => loop(e,r) }}, r.get, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2) =
        fn(fn, { try a(rec,x1,x1b,x2,x2b,r) catch { case e: TC => loop(e,r) }}, r.get, r)
    }
    def decrement(x: Rt2, by: Double): Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double =
        { try x(rec, r) catch { case e: TC => loop(e,r) }} - by
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double =
        { try x(rec,x1,x1b,r) catch { case e: TC => loop(e,r) }} - by
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double =
        { try x(rec,x1,x1b,x2,x2b,r) catch { case e: TC => loop(e,r) }} - by
    }

    val fib = new Rt2 {
      val body =
        if0(x1, num(0.0),
                if1(x1,
                    num(1.0),
                    plus(apRec(decrement(x1, 1.0)), apRec(decrement(x1, 2.0)))))

      def apply(rec: Rt2, r: R2) = ???
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double = body(rec, x1, x1b, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double = ???
    }
    val fibN = ap(fib, num(N))
  }

  println(normalize(builtins)(fib))
  println(normalize(builtins)(Compiled(manuallyCompiledFib)(Num(N))))
  println(fibScala(N))
  println(Rt2.eval(null, Rt2.fibN, R2(null)))

  val compiledFib = compile(builtins)(fib)

  QuickProfile.suite(
    QuickProfile.timeit("manually-compiled (3)", 0.08) {
      (Rt2.eval(null, Rt2.fibN, R2(null)) + math.random).toLong
    },
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
