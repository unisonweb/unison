package org.unisonweb

import org.unisonweb.EasyTest._
import org.unisonweb.util.UtilTests

object AllTests {
  val tests = suite(
    DecompileTests.tests,
    UtilTests.tests
  )
}

object RunAllTests extends App {
  run()(AllTests.tests)
}
