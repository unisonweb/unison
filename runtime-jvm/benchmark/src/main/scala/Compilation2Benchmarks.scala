package org.unisonweb.benchmark

import org.unisonweb._
import compilation2._
import Term.Term

object Compilation2Benchmarks {

  import QuickProfile.{profile, suite}

  def N(n: Int): Int = math.random.ceil.toInt * n

  val stackB = new Array[B](1024)
  val stackU = new Array[U](1024)
  val r = Result()
  val top = new StackPtr(-1)

  def runTerm(t: Term): Value.Lambda =
    run(compileTop(Lib2.builtins)(t)).asInstanceOf[Value.Lambda]

  def main(args: Array[String]): Unit = {
    suite(
      profile("scala-triangle") {
        def triangle(n: Int, acc: Int): Int =
          if (n == 0) acc else triangle(n - 1, acc + n)
        triangle(N(10000), N(0))
      },
      {
        val p = runTerm(Terms.triangle)
        profile("unison-triangle") {
          evalLam(p, r, top, stackU, N(10000), N(0), stackB, null, null).toLong
        }
      }
    )
    suite(
      profile("scala-fib") {
        def fib(n: Int): Int =
          if (n < 2) n else fib(n - 1) + fib(n - 2)
        fib(N(21))
      },
      {
        val p = runTerm(Terms.fib)
        profile("unison-fib") {
          evalLam(p, r, top, stackU, U0, N(21).toDouble, stackB, null, null).toLong
        }
      }
    )
    suite(
      profile("scala-fibPrime") {
        def fibPrime(n: Int): Int =
          if (n < 2) n else fibPrime2(n - 1) + fibPrime2(n - 2)
        def fibPrime2(n: Int): Int =
          if (n < 2) n else fibPrime(n - 1) + fibPrime(n - 2)
        fibPrime(N(21))
      },
      {
        val p = runTerm(Terms.fibPrime)
        profile("unison-fibPrime") {
          evalLam(p, r, top, stackU, U0, N(21), stackB, null, null).toLong
        }
      }
    )
  }
}
