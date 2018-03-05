package org.unisonweb.benchmark

import org.unisonweb.Term
import org.unisonweb.Term._
import org.unisonweb.compilation._
import org.unisonweb.benchmark.Builtins._

object Fib extends App {

  /** Compile and evaluate a term, the return result back as a term. */
  def normalize(builtins: Name => Computation)(e: Term): Term = {
    val rt = compile(builtins)(e)
    val r = Result()
    decompileSlot(evaluate(rt, r), r.boxed)
  }
  def decompileSlot(unboxed: D, boxed: P): Term =
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
  val builtin = (4.0: Term) + (5.0: Term)
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

  def sum1toN(n: Double) = LetRec("sum1toN" ->
    Lam('n, 'acc)(
      If0('n, 'acc, 'sum1toN.v('n.v - 1, 'acc.v + 'n.v))
    )
  )('sum1toN.v(n, 0))


  //  @annotation.tailrec
//  def iterateWhile[A](a: A)(f: A => A, ok: A => Boolean): A =
//    if (ok(a)) iterateWhile(f(a))(f, ok)
//    else a
//  iterateWhile a f ok = if ok a then iterateWhile (f a) f ok else a
//  @refried was thinking let’s compare iterateWhile(0.0)(_ + 1.0, _ < 1e6) in Scala vs Unison runtime

  def iterateWhile(max: Double) =
    LetRec(
      "iterateWhile" ->
        Lam('a, 'f, 'stop)(
          If0('stop.v('a), 'a, 'iterateWhile.v('f.v('a), 'f, 'stop))
        )
    )('iterateWhile.v(0.0, Lam('a)('a.v + 1.0), Lam('a)('a.v < max)))

  @annotation.tailrec def iterateWhileScala[A](a: A)(f: A => A, ok: A => Boolean): A =
    if (ok(a)) iterateWhileScala(f(a))(f, ok)
    else a

  val add1 = (_: Double) + 1.0
  def iterateWhileScala0(max: Double) =
    iterateWhileScala(0.0)(add1, _ < max)

  List(
    "applyIdentity" -> applyIdentity -> 3.0,
    "identityInLet" -> identityInLet -> 3.0,
    "identityInLet2" -> identityInLet2 -> 3.0,
    "identityInLetRec" -> identityInLetRec -> 3.0,
    "identityInLetRec2" -> identityInLetRec2 -> 3.0,
    "builtin" -> builtin -> 9.0,
    "countFrom" -> countFrom -> 50.0,
    "fib" -> fib -> 610.0,
    "lambdaFvs" -> lambdaFvs -> 78.0,
    "first" -> first(3, 4) -> 3.0,
    "second" -> second(3, 4) -> 4.0,
    "partiallyAppliedFirst" -> first(3)(4) -> 3.0,
    "partiallyAppliedSecond" -> second(3)(4) -> 4.0,
    "facTailRec" -> facTailRec -> 3628800.0,
    "facRec" -> facRec -> 3628800.0,
    "iterateWhile(...)" -> iterateWhile(2) -> 2.0,
    "sum1toN" -> sum1toN(N) -> 120.0,
    "ANF(sum1ToN)" -> Term.ANF(sum1toN(N)) -> 120.0
  ).foreach {
    case ((name, term), d) =>
      print(f"$name%20s:\t")
      val result = normalize(builtins)(term)
      println(f"$result%10s\texpected: $d%10.1f")
  }

  import QuickProfile._
  QuickProfile.suite(
    { val compiled = compile(builtins)(iterateWhile(5000.0))
      profile("iterateWhile(5000)") {
        evaluate(compiled, Result()).toLong + math.random.toLong
      }
    }
    ,{
      profile("iterateWhileScala(5000)") {
        iterateWhileScala0(5000).toLong + math.random.toLong
      }
    }
  )
  println()
  println(sum1toN(N))
  println()
  println(Term.ANF(sum1toN(N)))

}
