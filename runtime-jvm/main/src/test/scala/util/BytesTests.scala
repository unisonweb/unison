package org.unisonweb.util

import org.unisonweb.EasyTest._

object BytesTests {

  val tests = suite("Bytes.Seq")(
    test("apply") { implicit T =>
      0 until 1000 foreach { i =>
        val bytes = replicate(i)(byte)
        val byteSeq = Bytes.Seq(bytes)
        0 until bytes.length foreach { j =>
          expect1(bytes(j) == byteSeq(j))
        }
      }
      ok
    },
    test("take") { implicit T =>
      0 until 100 foreach { i =>
        val m = intIn(-3, i+3)
        val bytes = replicate(i)(byte)
        val byteSeq = Bytes.Seq(bytes)
        val bytest = bytes.take(m)
        val byteSeqt = byteSeq.take(m)
        expect1 { byteSeqt.toVector == bytest }
      }
      ok
    }
  )

}
