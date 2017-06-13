package org.unisonweb.benchmark

object QuickProfile {

  def timeit(label: String, threshold: Double = 0.05)(action: => Long): (String, Double) = {
    var N = 16L
    var i = 0
    var startTime = System.nanoTime
    var stopTime = System.nanoTime
    var sample = 1e9
    var K = 0L
    var ok = true
    var percentDeviation = Double.PositiveInfinity
    while (ok) {
      // try to increase N to get at least 100ms sample
      if (sample*N < 1e8) {
        val N2 = N * (1e8 / (sample*N)).toLong
        if (N == N2) N = (N.toDouble*1.2).toLong
        else N = N2
      }
      // otherwise increase N gradually to decrease variance
      else N = (N.toDouble*1.2).toLong
      print(s"\r * $label: ${formatNanos(sample)}, N=$N, deviation=$percentDeviation%, target deviation: ${threshold*100}%   ")
      val Y = 10
      val samples = (0 until Y) map { _ =>
        i = 0 ; val startTime = System.nanoTime
        while (i < N) { K += action; i += 1 }
        val stopTime = System.nanoTime
        val sample = (stopTime - startTime) / N
        print(" ")
        System.gc()
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

  def suite(s: (String,Double)*) {
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