package org.unisonweb.benchmark

object NonTailRecBenchmarks extends App {
  import QuickProfile.{suite, profile}

  // fib(0) = 0
  // fib(1) = 1
  // fib(2) = 1
  // fib(3) = 2 etc

  val N = 15.0

  def fib(n: Double): Double =
    if (n < 2.0) n
    else fib(n-1.0) + fib(n-2.0)

  def fib2(n: Double): Double =
    if (n == 0.0) 0.0
    else if (n == 1.0) 1.0
    else fib2(n-1.0) + fib2(n-2.0)

  def generalizedFib[A](n: Double)(isBase: Double => Boolean, base: Double => A, combine: (A,A) => A): A =
    if (isBase(n)) base(n)
    else combine(generalizedFib(n - 1.0)(isBase, base, combine),
                 generalizedFib(n - 2.0)(isBase, base, combine))

  def generalizedFib2[A](a0: A, a1: A, n: A)(
    f0: A => A, f1: A => A, combine: (A,A) => A, equal: (A,A) => Boolean): A =
    if (equal(n,a0)) a0
    else if (equal(n,a1)) a1
    else combine(generalizedFib2(a0, a1, f0(n))(f0, f1, combine, equal),
                 generalizedFib2(a0, a1, f1(n))(f0, f1, combine, equal))

  def generalizedFib3[A](a0: A, a1: A, n: A)(
    f0: A => A, f1: A => A, combine: (A,A) => A): A =
    if (n == a0) a0
    else if (n == a1) a1
    else combine(generalizedFib3(a0, a1, f0(n))(f0, f1, combine),
                 generalizedFib3(a0, a1, f1(n))(f0, f1, combine))

  // generalized fib is about 10x slower than native fib
  suite(
    profile("generalized fib (universal equals)") {
      generalizedFib3[Double](0.0, 1.0, (N + math.random).floor)(_ - 1.0, _ - 2.0, _ + _).toLong
    },
    profile("generalized fib (no universal equals)") {
      generalizedFib2[Double](0.0, 1.0, (N + math.random).floor)(_ - 1.0, _ - 2.0, _ + _, _ == _).toLong
    },
    profile("native fib (==)") { (fib2((N + math.random).floor)).toLong }
    //profile("generalized fib (no universal equals)") {
    //  generalizedFib[Double]((N + math.random).floor)(_ < 2.0, n => n, _ + _, _ == _).toLong
    //},
    //profile("native fib") { (fib((N + math.random).floor)).toLong }
    // profile("math.random", 0.08) { math.random.toLong },
    // profile("noop", 0.8) { 1L },
  )
}
