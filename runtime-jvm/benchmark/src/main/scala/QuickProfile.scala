package org.unisonweb.benchmark

/**
 * Simple-to-use, fast, and relatively accurate benchmarking functions.
 *
 * `profile` runs an individual benchmark and reports performance.
 * `suite` runs a collection of benchmarks and reports relative performance.
 *
 * Unlike JMH, we do not require picking an arbitrary number of warmup iterations
 * or recorded iterations (often chosen to be either too small, yielding
 * inaccurate or wildly varying results, or too big, leading benchmarks to take
 * forever and not be run as part of normal development).
 *
 * See `profile` docs for more details on the methodology and its limitations.
 *
 * Example usage: {{{
     import QuickProfile.{suite, profile}
     suite(
       profile("loop1") {
         val n = 1e6 + math.random
         while (n > 0.0) n -=1
         n.toLong
       },
       profile("loop2") {
         val n = 1e6 + math.random;
         (0 until 1000000).foreach { _ => n -= 1.0 }
         n.toLong
       },
       { // setup for the benchmark, won't be measured
         val nums = Vector.range(0, 1000000)
         // okay, start measuring
         profile("loop3") {
           val n = 1e6 + math.random;
           nums.foreach { _ => n -= 1.0 }
           n.toLong
         }
       }
     )
   }}}

 Which produces output like: {{{

   - loop1:  1.475 milliseconds (4.3% deviation, N=68, K = 0)
   - loop2:  1.069 milliseconds (4.3% deviation, N=224, K = 0)
   - loop3:  14.776 milliseconds (3.0% deviation, N=26, K = 0)
  1.0             loop2
  1.37            loop1
  13.81           loop3

 }}}

*/
object QuickProfile {

  /**
   * Run an action repeatedly, capturing profiling info, until the % deviation of
   * timing info is less than `threshold` (closer to 0). Idea is to discover the
   * steady state of performance that occurs when most hot spots have been JIT'd
   * without needing to pick an arbitrary number of warmup iterations and trials
   * (which are often either too small, leading to inaccurate results, or too big,
   * leading to profiling taking way too long).
   *
   * This function increases N--the number of times `action` is invoked per
   * iteration--until each iteration takes at least 100ms. Once reaching this point,
   * it then gradually increase N exponentially (N = N * (1 + epsilon)) until percent
   * deviation drops below `threshold`. This all tends to happen pretty quickly.
   *
   * The `action` must return a `Long`, preferably unique for each execution, and the
   * sum of these numbers is threaded through the profiling computation to prevent the
   * JVM from doing any heroic optimizations that would eliminate executions of `action`.
   *
   * One caveat: JVM optimizations are not totally deterministic, so running the same
   * benchmark with a fresh JVM may reach a different steady state (though if performance
   * is highly sensitive to this, it could be a good idea to find a different way of
   * expressing your program such performance is not as fragile). It's usually
   * obvious from a handful of runs of a benchmark whether any nondeterminism of JVM
   * optimizations is relevant for performance, but for maximum accuracy it can be a good
   * idea to average results from multiple JVM runs.
   */
  def profile(label: String, threshold: Double = 0.05)(action: => Long): (String, Double) = {
    var N = 16L
    var i = 0
    var startTime = System.nanoTime
    var stopTime = System.nanoTime
    var sample = 1e9
    var K = 0L
    var ok = true
    var percentDeviation = Double.PositiveInfinity
    while (ok) {
      // try to increase N to get at least 100ms sample -
      if (sample*N < 1e8) { // 1e8 nanos is 100ms
        // do linear interpolation to guess N that will hit 100ms exactly
        val N2 = N * (1e8 / (sample*N)).toLong
        if ((N.toDouble / N2.toDouble - 1.0).abs < .15)
          // we're close enough, stop interpolating and just grow N exponentially
          N = (N.toDouble*1.2).toLong
        else
          // not close enough, so use the linear interpolation
          N = N2
      }
      // otherwise increase N gradually to decrease variance
      else N = (N.toDouble*1.2).toLong
      print(s"\r * $label: ${formatNanos(sample)}, N=$N, deviation=$percentDeviation%, target deviation: ${threshold*100}%   ")
      val Y = 10 //
      val samples = (0 until Y) map { _ =>
        i = 0 ; val startTime = System.nanoTime
        // note - we sum the `Long` values returned from each `action`, to ensure
        // `action` cannot be optimized away
        while (i < N) { K += action; i += 1 }
        val stopTime = System.nanoTime
        val sample = (stopTime - startTime) / N
        print(" ")
        System.gc() // try to minimize variance due to GC timing
        sample
      }
      val mean = samples.sum / Y.toDouble
      val variance = samples.map(x => math.pow(x.toDouble - mean, 2)).sum / Y
      val stddev = math.sqrt(variance)
      val v = stddev / mean
      percentDeviation = (v * 1000).toInt.toDouble / 10
      if (v <= threshold) {
        ok = false
        // println("% deviation below threshold: " + v)
      }
      else {
        // println("% deviation too high, increasing trials: " + v)
      }
      sample = mean
    }
    println("\r - "+label + ":  " + formatNanos(sample) + s" ($percentDeviation% deviation, N=$N, K = ${K.toString.take(3)})                         ")
    (label, sample)
  }

  def roundToThousands(n: Double) = (n * 1000).toInt / 1000.0
  def roundToHundreds(n: Double) = (n * 100).toInt / 100.0

  // def formatNanos(nanos: Double) = nanos.toString
  def formatNanos(nanos: Double) = {
    if (nanos > 1e9) roundToThousands(nanos/1e9).toString + " seconds"
    else if (nanos > 1e6) roundToThousands(nanos/1e6).toString + " milliseconds"
    else if (nanos > 1e3) roundToThousands(nanos/1e3).toString + " microseconds"
    else nanos.toString + " nanoseconds"
  }

  def suite(s: (String,Double)*): Unit = {
    val tests = s.toList.sortBy(_._2)
    val minNanos = tests.head._2
    // min * x = y
    // x = y / min
    tests.foreach { case (label, nanos) =>
      val x = roundToHundreds(nanos / minNanos)
      println(x.toString.padTo(16, " ").mkString + label)
    }
  }
}
