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
    {
      type V = AnyRef
      type D = Double
      case object SelfTC extends Throwable { override def fillInStackTrace = this }
      case class Result(var boxed: V, var fn: V, var x0: D, var x0b: V, var x1: D, var x1b: V,
                        var unused: V = null, var unused2: V = null,
                        var unused3: Array[AnyRef] = null)
      type R = Result
      abstract class Lambda {
        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, r: R): D
        // def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, r: R): D = ???
      }
      val x0var = new Lambda {
        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, r: R): D = {
          if (x0b eq null) { r.boxed = null; x0 }
          else ???
        }
      }
      val x1var = new Lambda {
        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, r: R): D = {
          if (x1b eq null) { r.boxed = null; x1 }
          else ???
        }
      }
      val plus = new Lambda {
        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, r: R): D = {
          r.boxed = null
          x0 + x1
        }
      }
      val minus = new Lambda {
        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, r: R): D = {
          r.boxed = null
          x0 - x1
        }
      }
      def num(d: Double) = new Lambda {
        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, r: R): D = {
          r.boxed = null
          d
        }
      }

      def foo(r: R) = ???
      val step = new Lambda {
        val one = num(1.0)
        def apply(rec: Lambda, n: D, x0b: V, acc: D, x1b: V, r: R): D =
          if ({ try x0var(rec, n, x0b, acc, x1b, r) catch { case SelfTC => foo(r) }} == 0.0)
            x1var(rec, n, x0b, acc, x1b, r)
          // if (n == 0) acc
          else {
            r.x0 =
              try minus(rec, x0var(rec, n, x0b, acc, x1b, r), r.boxed,
                         one(rec, n, x0b, acc, x1b, r), r.boxed,
                         r)
              catch { case SelfTC => foo(r) }
            r.x0b = r.boxed
            r.x1 =
              try
                plus(rec, x0var(rec, n, x0b, acc, x1b, r), r.boxed,
                          x1var(rec, n, x0b, acc, x1b, r), r.boxed, r)
              catch { case SelfTC => foo(r) }
            r.x1b = r.boxed
            r.unused = null
            r.unused2 = null
            r.unused3 = null
            throw SelfTC
         }
      }
      // sum1toN n acc = if n == 0 then acc else sum1toN (n - 1) (acc + n)

      val sum1toN = new Lambda {
        def apply(rec: Lambda, n0: D, x0b: V, acc0: D, x1b: V, r: R): D = {
          var n = n0
          var acc = acc0
          var unused1 = x0b
          var unused2 = x1b
          while (true) {
            try return step(step, n, x0b, acc, x1b, r)
            catch { case SelfTC =>
              n = r.x0
              acc = r.x1
              unused1 = r.x0b
              unused2 = r.x1b
            }
          }
          0.0
        }
      }
      println {
        val r = Result(null, null, 0.0, null, 0.0, null)
        "sum1toN sanity check: " + sum1toN(sum1toN, 4.0, null, 0.0, null, r).toLong
      }
      profile("unison-based exception loop (more realistic)") {
        val r = Result(null, null, 0.0, null, 0.0, null)
        sum1toN(sum1toN, (N + math.random).floor, null, 0.0, null, r).toLong
      }
    },
    {
      type V = AnyRef
      type D = Double
      case object SelfTC extends Throwable { override def fillInStackTrace = this }
      case class Result(var boxed: V, var fn: V, var x0: D, var x0b: V, var x1: D, var x1b: V)
      type R = Result
      abstract class Lambda {
        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, r: R): D
        // def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, r: R): D = ???
      }
      val x0var = new Lambda {
        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, r: R): D = x0
      }
      def foo(r: R) = ???
      val step = new Lambda {
        def apply(rec: Lambda, n: D, x0b: V, acc: D, x1b: V, r: R): D =
          if ({ try x0var(rec, n, x0b, acc, x1b, r) catch { case SelfTC => foo(r) }} == 0.0) acc
          // if (n == 0) acc
          else { r.x0 = n - 1; r.x1 = acc + n; throw SelfTC }
      }
      // sum1toN n acc = if n == 0 then acc else sum1toN (n - 1) (acc + n)

      val sum1toN = new Lambda {
        def apply(rec: Lambda, n0: D, x0b: V, acc0: D, x1b: V, r: R): D = {
          var n = n0
          var acc = acc0
          var unused1 = x0b
          var unused2 = x1b
          while (true) {
            try return step(step, n, x0b, acc, x1b, r)
            catch { case SelfTC =>
              n = r.x0
              acc = r.x1
              unused1 = r.x0b
              unused2 = r.x1b
            }
          }
          0.0
        }
      }
      profile("unison-based exception loop (1)") {
        val r = Result(null, null, 0.0, null, 0.0, null)
        sum1toN(sum1toN, (N + math.random).floor, null, 0.0, null, r).toLong
      }
    },
    {
      import org.unisonweb.Term._
      import Builtins.{Arithmetic, builtins}
      val sum1toN = LetRec("sum1toN" ->
        Lam('n, 'acc)(
          If0('n, 'acc, 'sum1toN.v('n.v - 1, 'acc.v + 'n.v))
        )
      )('sum1toN)
      import org.unisonweb.compilation.{compile, Result, Lambda}
      val compiled = compile(builtins)(sum1toN)
      val r = Result()
      // todo: could be faster if letrec detected that the only recursive calls are selfcalls,
      // and skipped saving a lazy self reference.  letrec1 used to do this, but the optimization
      // shouldn't be limited to a letrec1 construction
      compiled(null, r)
      val lambda = r.boxed.asInstanceOf[Lambda]
      profile("unison loop") {
        lambda(null, 0.0, null, (N + math.random).floor, null, r).toLong
      }
    },
    //{
    //  // only difference between this and above is the while loop is encoded via
    //  // an @annotation.tailrec function
    //  type V = AnyRef
    //  type D = Double
    //  case object SelfTC extends Throwable { override def fillInStackTrace = this }
    //  case class Result(var boxed: V, var fn: V, var x0: D, var x0b: V, var x1: D, var x1b: V)
    //  type R = Result
    //  abstract class Lambda {
    //    def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, r: R): D
    //    // def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, r: R): D
    //  }
    //  val step = new Lambda {
    //    def apply(rec: Lambda, n: D, x0b: V, acc: D, x1b: V, r: R): D =
    //      if (n == 0) acc
    //      else { r.x0 = n - 1; r.x1 = acc + n; throw SelfTC }
    //  }
    //  // sum1toN n acc = if n == 0 then acc else sum1toN (n - 1) (acc + n)

    //  // mysteriously does not compile, scalac reports there is a call in non-tail position
    //  // (but won't tell me what it is)
    //  @annotation.tailrec
    //  def loop(x0: D, x0b: V, x1: D, x1b: V, r: R): D =
    //    try step(step, x0, x0b, x1, x1b, r)
    //    catch { case e: SelfTC.type => loop(r.x0, r.x0b, r.x1, r.x1b, r) }

    //  val sum1toN = new Lambda {
    //    def apply(rec: Lambda, n0: D, x0b: V, acc0: D, x1b: V, r: R): D =
    //      loop(n0, x0b, acc0, x1b, r)
    //  }
    //  profile("unison-based exception loop (2)") {
    //    val r = Result(null, null, 0.0, null, 0.0, null)
    //    sum1toN(sum1toN, (N + math.random).floor, null, 0.0, null, r).toLong
    //  }
    //},
    profile("double-based loop") {
      var n = (N + math.random).floor
      var sum = 0.0
      def go(): Long = { while (true) {
        sum += n
        n -= 1.0
        if (n == 0.0) return sum.toLong
      }; 0L }
      go()
    }
    //profile("iterateWhile") { iterateWhile(math.random)(_ + 1.0, _ < N).toLong },
    //profile("iterateWhile2") { iterateWhile2(N, math.random)((n,a) => n - 1.0, (n,a) => n + a, _ > 0.0).toLong },
    //{
    //  case object TC extends Throwable { override def fillInStackTrace = this }
    //  case class Result(var boxed: AnyRef = null,
    //                    var tailCall: AnyRef = null,
    //                    var n: Double = 0.0,
    //                    var nb: AnyRef = null,
    //                    var sum: Double = 0.0,
    //                    var sumb: AnyRef = null)

    //  profile("ex-based loop (1a)") {
    //    var n = N + math.random
    //    var sum = 0.0
    //    def step0(n: Double, sum: Double, r: Result): Long = {
    //      r.n = n - 1.0
    //      r.sum = sum + n
    //      if (n > 0.0) throw TC
    //      else return r.sum.toLong
    //    }
    //    val r = Result(n = N)
    //    def go(): Long = { while (true) {
    //    // def go(): Long = { val r = Result(n = N); while (true) { // 50x slower, todo: why??
    //      try { return step0(r.n, r.sum, r) }
    //      // catch { case TC => n = r.n; sum = r.sum } // this is slower than just letting r have loop state
    //      catch { case TC => () }
    //    }; 0L }
    //    go()
    //  }
    //},
    //{
    //  case object TC extends Throwable { override def fillInStackTrace = this }
    //  case class Result(var boxed: AnyRef = null,
    //                    var tailCall: AnyRef = null,
    //                    var n: Double = 0.0,
    //                    var nb: AnyRef = null,
    //                    var sum: Double = 0.0,
    //                    var sumb: AnyRef = null)

    //  profile("ex-based loop (1b)") {
    //    var n = N + math.random
    //    var sum = 0.0
    //    def step0(n: Double, sum: Double, r: Result): Long = {
    //      r.n = n - 1.0
    //      r.sum = sum + n
    //      if (n > 0.0) throw TC
    //      else return r.sum.toLong
    //    }
    //    val r = Result(n = N)
    //    def go(): Long = { val r = Result(n = N); while (true) { // 50x slower, todo: why??
    //      try { return step0(r.n, r.sum, r) }
    //      catch { case TC => () }
    //    }; 0L }
    //    go()
    //  }
    //},
    //{
    //  case object TC extends Throwable { override def fillInStackTrace = this }

    //  profile("ex-based loop (1)") {
    //    var n = N + math.random
    //    var sum = 0.0
    //    while (n > 0.0) {
    //      try { sum += n; n -= 1; throw TC }
    //      catch { case TC => () }
    //    }
    //    sum.toLong
    //  }
    //},
    //{
    //  def step(n: Double, sum: Double) = throw new TC(n - 1, sum + n)
    //  class TC(val n: Double, val sum: Double) extends Throwable {
    //    override def fillInStackTrace = this
    //  }
    //  profile("ex-based loop (2)") {
    //    var n = N + math.random
    //    var sum = 0.0
    //    while (n > 0.0) {
    //      sum += n
    //      try step(n, sum)
    //      catch { case tc: TC => n = tc.n; sum = tc.sum }
    //    }
    //    sum.toLong
    //  }
    //},
    //profile("long-based loop") {
    //  var n = N.toLong + (math.random * 2).toLong
    //  var sum = 0L
    //  def go(): Long = { while (true) {
    //    sum += n
    //    n -= 1L
    //    if (n < 0) return sum
    //  }; 0L }
    //  go()
    //},
  )
}

