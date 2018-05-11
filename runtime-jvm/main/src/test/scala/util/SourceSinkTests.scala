package org.unisonweb.util

import org.unisonweb.EasyTest._

object SourceSinkTests {
  val tests =
    test("source/sink") { implicit T =>
      1 until 100 foreach { n =>
        val input = byteArray(intIn(1,n+1*2)).toList
        val bytes = Sink.toChunks(5) { sink =>
          input foreach { sink putByte _ }
        }
        val bytes2 = {
          val src = Source.fromChunks(intIn(1,n+1*2))(bytes)
          List.fill(input.size)(src.getByte)
        }
        equal1(bytes2, bytes.toList.flatten)
      }
      ok
    }
}

