package org.unisonweb.util

import org.unisonweb.EasyTest._

object CodecsTests {
  val tests = suite("Codecs") {
    test("read/writeLong") { implicit T =>
      0 until 1000 foreach { n =>
        val n = long
        expect1 { Codecs.readLong(Codecs.writeLong(n)) == n }
      }
      ok
    }
  }
}
