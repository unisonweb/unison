package org.unisonweb.benchmark

/*
Some benchmarks to test performance of different ways of encoding loops.
Summary is that loops using tails calls encoded as nullary exceptions
can have performance approaching native while loops.

Observation: polymorphic functions like `iterateWhile`
can still run as fast as while loops (hypothesis is that this is
because everything can be inlined and escape analysis can take care
of any boxing / unboxing that would otherwise happen).

Observation: keeping the loop state in the Result type (rather than
copying to local vars) had more robust performance, maybe? Depends on run.

Observation: long-based loops might be a tad faster.

Sample output:

   - iterateWhile:  1.537 milliseconds (4.0% deviation, N=38, K = 570)
   - iterateWhile2:  1.669 milliseconds (2.6% deviation, N=57, K = 380)
   - ex-based loop (1a):  3.887 milliseconds (4.3% deviation, N=22, K = 205)
   - ex-based loop (1b):  4.103 milliseconds (2.2% deviation, N=74, K = 183)
   - ex-based loop (1):  2.977 milliseconds (3.2% deviation, N=1111, K = 330)
   - ex-based loop (2):  13.081 milliseconds (3.8% deviation, N=26, K = 670)
   - double-based loop:  3.761 milliseconds (3.3% deviation, N=31, K = 490)
   - long-based loop:  2.752 milliseconds (4.4% deviation, N=26, K = 335)
  1.0             iterateWhile
  1.08            iterateWhile2
  1.78            long-based loop
  1.93            ex-based loop (1)
  2.44            double-based loop
  2.52            ex-based loop (1a)
  2.66            ex-based loop (1b)
  8.5             ex-based loop (2)

*/
object LoopBenchmarks extends App {
  import QuickProfile.{suite, profile}

  val N = 1e6

  @annotation.tailrec
  def iterateWhile[A](a: A)(f: A => A, ok: A => Boolean): A =
    if (ok(a)) iterateWhile(f(a))(f, ok)
    else a

  @annotation.tailrec
  def iterateWhile2[S,A](s: S, a: A)(sf: (S, A) => S, sa: (S, A) => A, ok: S => Boolean): A =
    if (ok(s)) iterateWhile2(sf(s,a), sa(s,a))(sf, sa, ok)
    else a

  suite(
    profile("iterateWhile") { iterateWhile(math.random)(_ + 1.0, _ < N).toLong },
    profile("iterateWhile2") { iterateWhile2(N, math.random)((n,a) => n - 1.0, (n,a) => n + a, _ > 0.0).toLong },
    {
      case object TC extends Throwable { override def fillInStackTrace = this }
      case class Result(var boxed: AnyRef = null,
                        var tailCall: AnyRef = null,
                        var n: Double = 0.0,
                        var nb: AnyRef = null,
                        var sum: Double = 0.0,
                        var sumb: AnyRef = null)

      profile("ex-based loop (1a)") {
        var n = N + math.random
        var sum = 0.0
        def step0(n: Double, sum: Double, r: Result): Long = {
          r.n = n - 1.0
          r.sum = sum + n
          if (n > 0.0) throw TC
          else return r.sum.toLong
        }
        val r = Result(n = N)
        def go(): Long = { while (true) {
        // def go(): Long = { val r = Result(n = N); while (true) { // 50x slower, todo: why??
          try { return step0(r.n, r.sum, r) }
          // catch { case TC => n = r.n; sum = r.sum } // this is slower than just letting r have loop state
          catch { case TC => () }
        }; 0L }
        go()
      }
    },
    {
      case object TC extends Throwable { override def fillInStackTrace = this }
      case class Result(var boxed: AnyRef = null,
                        var tailCall: AnyRef = null,
                        var n: Double = 0.0,
                        var nb: AnyRef = null,
                        var sum: Double = 0.0,
                        var sumb: AnyRef = null)

      profile("ex-based loop (1b)") {
        var n = N + math.random
        var sum = 0.0
        def step0(n: Double, sum: Double, r: Result): Long = {
          r.n = n - 1.0
          r.sum = sum + n
          if (n > 0.0) throw TC
          else return r.sum.toLong
        }
        val r = Result(n = N)
        def go(): Long = { val r = Result(n = N); while (true) { // 50x slower, todo: why??
          try { return step0(r.n, r.sum, r) }
          catch { case TC => () }
        }; 0L }
        go()
      }
    },
    {
      case object TC extends Throwable { override def fillInStackTrace = this }

      profile("ex-based loop (1)") {
        var n = N + math.random
        var sum = 0.0
        while (n > 0.0) {
          try { sum += n; n -= 1; throw TC }
          catch { case TC => () }
        }
        sum.toLong
      }
    },
    {
      def step(n: Double, sum: Double) = throw new TC(n - 1, sum + n)
      class TC(val n: Double, val sum: Double) extends Throwable {
        override def fillInStackTrace = this
      }
      profile("ex-based loop (2)") {
        var n = N + math.random
        var sum = 0.0
        while (n > 0.0) {
          sum += n
          try step(n, sum)
          catch { case tc: TC => n = tc.n; sum = tc.sum }
        }
        sum.toLong
      }
    },
    profile("double-based loop") {
      var n = N + math.random
      var sum = 0.0
      def go(): Long = { while (true) {
        sum += n
        n -= 1.0
        if (n < 0.0) return sum.toLong
      }; 0L }
      go()
    },
    profile("long-based loop") {
      var n = N.toLong + (math.random * 2).toLong
      var sum = 0L
      def go(): Long = { while (true) {
        sum += n
        n -= 1L
        if (n < 0) return sum
      }; 0L }
      go()
    },
  )
}

