package org.unisonweb.benchmark

object NonTailRecBenchmarks extends App {
  import QuickProfile.{suite, profile}

  // fib(0) = 0
  // fib(1) = 1
  // fib(2) = 1
  // fib(3) = 2 etc

  val N = 15.0

  def fib(n: Double): Double =
    if (n == 0.0) 0
    else if (n == 1.0) 1.0
    else fib(n-1.0) + fib(n-2.0)

  def generalizedFib[A](a0: A, a1: A, n: A)(f0: A => A, f1: A => A, combine: (A,A) => A): A =
    if (n == a0) a0
    else if (n == a1) a1
    else combine(generalizedFib(a0, a1, f0(n))(f0, f1, combine),
                 generalizedFib(a0, a1, f1(n))(f0, f1, combine))

  // generalized fib is about 10x slower than native fib
  suite(
    profile("generalized fib", 0.04) {
      generalizedFib(0.0, 1.0, (N + math.random).floor)(_ - 1.0, _ - 2.0, _ + _).toLong
    },
    profile("native fib", 0.04) { (fib((N + math.random).floor)).toLong }
    // profile("math.random", 0.08) { math.random.toLong },
    // profile("noop", 0.8) { 1L },
  )
}
