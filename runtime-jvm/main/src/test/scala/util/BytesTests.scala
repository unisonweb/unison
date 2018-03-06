package org.unisonweb.util

import org.unisonweb.EasyTest._

object BytesTests {

  val tests = test("Bytes.Seq") { implicit T =>
    0 until 1000 foreach { i =>
      val bytes = replicate(i)(byte)
      val byteSeq = Bytes.Seq(bytes)
      0 until bytes.length foreach { j =>
        expect1(bytes(j) == byteSeq(j))
      }
    }
    ok
  }

}
