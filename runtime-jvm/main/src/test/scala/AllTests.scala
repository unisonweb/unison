package org.unisonweb

import org.unisonweb.EasyTest._
import org.unisonweb.util.UtilTests

object AllTests {
  val tests = suite(
    DecompileTests.tests,
    UtilTests.tests,
    CompilationTests.tests
  )
}

object RunAllTests {
  def main(args: Array[String]) = {
    val prefix = if (args.length == 1) args(0) else ""
    run(prefix = prefix)(AllTests.tests)
  }
}
