package org.unisonweb.util

import org.unisonweb.EasyTest._

object SerializationTests {
  val tests = suite("Source/Sink") {
    test("read/writeLong") { implicit T =>
      0 until 1000 foreach { n =>
        val n = long
        expect1 { Source.readLong(Sink.writeLong(n)) == n }
      }
      ok
    }
  }
}
