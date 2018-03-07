package org.unisonweb.util

import org.unisonweb.EasyTest._

object UtilTests {

  lazy val tests = scope("util") {
    suite(DequeTests.tests,
          SequenceTests.tests,
          BytesTests.tests,
          TextTests.tests,
          CritbyteTests.tests)
  }
}

object RunUtilTests {
  def main(args: Array[String]) =
    //run()(UtilTests.tests)
    run(prefix = "util.Critbyte")(UtilTests.tests)
}

