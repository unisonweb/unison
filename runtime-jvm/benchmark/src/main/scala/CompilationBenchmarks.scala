package org.unisonweb.benchmark

import org.unisonweb.Term.Term
import org.unisonweb._
import org.unisonweb.compilation._
import org.unisonweb.util.Unboxed.F2.{DD_D, LL_L}
import org.unisonweb.util.{Stream, Unboxed}

object CompilationBenchmarks {

  import QuickProfile.{profile, suite}

  def N(n: Int): Int = math.random.ceil.toInt * n
  val env = Environment(
    Builtins.builtins,
    Map.empty,
    BuiltinTypes.dataConstructors,
    BuiltinTypes.effects)

  val stackB = new Array[B](1024)
  val stackU = new Array[U](1024)
  val r = Result()
  val top = StackPtr.empty

  def runTerm(t: Term): Value.Lambda =
    run(compileTop(env)(t)).asInstanceOf[Value.Lambda]

  val triangleCount = 100000

  def main(args: Array[String]): Unit = {
    assert(false) // making sure assertions are disabled
    suite(
      profile("triangle scala tailrec function") {
        def triangle(n: Int, acc: Int): Int =
          if (n == 0) acc else triangle(n - 1, acc + n)
        triangle(N(triangleCount), N(0))
      },
      { val s = scala.Stream.range(0, N(triangleCount))
        profile("triangle scala-stream .foldLeft(_ + _)") { s.foldLeft(N(0))(_ + _).toLong }
      },
      {
        val p = runTerm(Terms.triangle)
        profile("triangle unison tailrec function") {
          evalLam(p, r, top, stackU, N(triangleCount), N(0), stackB, UnboxedType.Int64, UnboxedType.Int64).toLong
        }
      },
      {
        profile("triangle stream .sumIntegers") {
          Stream.from(N(0)).take(N(triangleCount)).sumIntegers
        }
      },
      {
        profile("triangle stream .foldLeft(LL_L)") {
          Stream.from(N(0)).take(N(triangleCount))
            .foldLeft0(0, null: Unboxed.Unboxed[U])(
              LL_L(_ + _))((u, _) => u)
        }
      },
      {
        profile("triangle stream .foldLeftTC(LL_L)") {
          Stream.from(N(0)).take(N(triangleCount))
            .foldLeft(0l)(LL_L(_ + _))
        }
      },
      {
        profile("triangle stream .foldLeftTC(DD_D)") {
          Stream.from(N(0).toDouble, by = 1.0).take(N(triangleCount))
            .foldLeft(0.0)(DD_D(_ + _)).toLong
        }
      },
      {
        val plusU = UnisonToScala.toUnboxed2(Builtins.Int64_add)

        val env = (new Array[U](20), new Array[B](20), StackPtr.empty, Result())
        profile("triangle stream .foldLeft(plusU)") {
          Stream.fromUnison(0).take(N(triangleCount))
            .foldLeft(Value(0))(plusU(env)) match {
            case Value.Unboxed(n, _) => n
          }
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
          evalLam(p, r, top, stackU, U0, N(21), stackB, null, UnboxedType.Int64).toLong
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
          evalLam(p, r, top, stackU, U0, N(21), stackB, null, UnboxedType.Int64).toLong
        }
      }
    )
  }
}
