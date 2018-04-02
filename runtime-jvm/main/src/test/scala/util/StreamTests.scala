package org.unisonweb.util

import org.unisonweb.EasyTest._
import org.unisonweb.compilation2.{U,U0}

object StreamTests {
  val tests = suite("Stream")(
    test("ex1") { implicit T =>
      equal(
        Stream.from(0.0).take(10000).sum(()),
        (0 until 10000).sum.toDouble)
    },
    test("toSequence") { implicit T =>
      equal(
        Stream.from(0.0).take(10000).toSequence(()){ (u, _) => u },
        Sequence.apply(0 until 10000: _*).map(_.toDouble)
      )
    },
    test("foldLeft-scalaPlus") { implicit T =>
      val plus: Unboxed.F2[Any, U, U, U] = Unboxed.F2.boxedScalaFunction(_ + _)
      equal(
        Stream.from(0.0).take(10000).box[U](identity)
          .foldLeft((), U0, U0)(plus)((_,a) => a),
        (0 until 10000).sum.toDouble
      )
    }
  )
}
