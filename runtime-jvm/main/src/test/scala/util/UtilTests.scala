package org.unisonweb.util

import org.unisonweb.EasyTest._

object UtilTests {

  val tests = scope("util") {
    suite(DequeTests.tests,
          SequenceTests.tests,
          BytesTests.tests,
          TextTests.tests,
          CritbyteTests.tests)
  }
}
