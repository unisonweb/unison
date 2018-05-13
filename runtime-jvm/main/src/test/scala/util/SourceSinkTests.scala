package org.unisonweb.util

import org.unisonweb.EasyTest._

object SourceSinkTests {
  val tests = suite("source/sink")(
    test("sink") { implicit T =>
      // sink should record all bytes
      1 until 100 foreach { n =>
        val input = byteArray(n).toVector
        val A = intIn(1,n+1*2)
        val bytes = Sink.toChunks(A) { sink =>
          var rem = input; while (rem.nonEmpty) {
            val n = intIn(1, rem.size + 1)
            val (rem1,rem2) = rem.splitAt(n)
            sink.put(rem1.toArray)
            rem = rem2
          }
        }
        equal1(bytes.toList.flatten, input.toList)
      }
      ok
    },
    test("source/sink") { implicit T =>
      // the sink should record all bytes, and the source
      1 until 10 foreach { n =>
        val input = byteArray(n).toVector
        val A = intIn(1,n+1*2)
        val bytes = Sink.toChunks(A) { sink =>
          var rem = input; while (rem.nonEmpty) {
            println("1: " + math.random)
            val n = intIn(1, rem.size + 1)
            val (rem1,rem2) = rem.splitAt(n)
            sink.put(rem1.toArray)
            rem = rem2
          }
          println("woot")
        }
        println("bytes: " + bytes)
        val B = intIn(1,n+1*2)
        note("A: " + A, true)
        note("B: " + B, true)
        val bytes2 = {
          val src = Source.fromChunks(B)(bytes)
          var acc = Vector.empty[Byte]
          var rem = input.size
          while (acc.length != input.length) {
            println(math.random)
            println(acc.length + " " + input.length)
            println
            val n = intIn(1, rem + 1)
            acc = acc ++ src.get(n).toVector
            rem -= n
          }
          acc
        }
        equal1(bytes2, bytes.toList.flatten)
        equal(bytes2, input)
      }
      ok
    }
  )
}

