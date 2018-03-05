package org.unisonweb.util

import org.unisonweb.EasyTest._

object UtilTests {

  lazy val tests = scope("util") { suite(DequeTests.tests, SequenceTests.tests, TextTests.tests) }
}

object RunUtilTests extends App {
  run()(UtilTests.tests)
}

