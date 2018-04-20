package org.unisonweb.util

import org.unisonweb.EasyTest._
import org.unisonweb._
import org.unisonweb.compilation._
import org.unisonweb.util.Unboxed.Unboxed
import org.unisonweb.util.Unboxed.F1.{L_B, D_B, L_L}
import org.unisonweb.util.Unboxed.F2.{LL_L,DD_D,LD_L}

object StreamTests {
  val tests = suite("Stream")(
    test("take/drop") { implicit T =>
      equal(
        Stream.from(0).take(5).drop(3).sumIntegers,
        (0 until 5).drop(3).sum
      )
    },
    test("ex1") { implicit T =>
      equal(
        Stream.from(0).take(10000).sumIntegers,
        (0 until 10000).sum)
    },
    test("map") { implicit T =>
      equal(
        Stream.from(0).take(10000).map(L_L(_ + 1)).sumIntegers,
        (0 until 10000).map(_ + 1).sum
      )
    },
    test("filter") { implicit T =>
      equal(
        Stream.from(0).take(10000).filter(L_B(_ % 2 == 0)).sumIntegers,
        (0 until 10000).filter(_.toDouble % 2 == 0).sum
      )
    },
    test("takeWhile") { implicit T =>
      equal(
        Stream.from(0).take(100).takeWhile(L_B(_ < 50)).sumIntegers,
        (0 until 100).takeWhile(_ < 50).sum
      )
      equal(
        Stream.from(0.0, by = 1.0).take(100).takeWhile(D_B(_ < 50.0)).sumFloats,
        (0.0 until 100.0 by 1.0).takeWhile(_ < 50.0).sum
      )
    },
    test("dropWhile") { implicit T =>
      equal(
        Stream.from(0).take(100).dropWhile(L_B(_ < 50)).sumIntegers,
        (0 until 100).dropWhile(_ < 50).sum
      )
    },
    test("zipWith") { implicit T =>
      val s1 = Stream.from(0)
      val s2 = scala.collection.immutable.Stream.from(0)
      equal(
        s1.zipWith(s1.drop(1))(LL_L(_ * _)).take(100).sumIntegers,
        s2.zip(s2.drop(1)).map { case (a,b) => a * b }.take(100).sum
      )
    },
    test("toSequence0") { implicit T =>
      equal(
        Stream.from(0).take(10000).toSequence0 { (u, _) => u },
        Sequence.apply(0 until 10000: _*)
      )
    },
    test("toSequence") { implicit T =>
      equal(
        Stream.from(0).take(10000).toSequence,
        Sequence.apply(0 until 10000: _*)
      )
    },
    test("foldLeft0-scala-plus-long") { implicit T =>
      equal(
        Stream.from(0).take(10000).foldLeft0(U0, null:Unboxed[Long])(
          LL_L(_ + _))((u,_) => u),
        (0 until 10000).sum
      )
    },
    test("foldLeft-scala-plus-long") { implicit T =>
      val plusU = UnisonToScala.toUnboxed2(Builtins.Integer_add)
      val env = (new Array[U](20), new Array[B](20), StackPtr.empty, Result())
      equal(
        Stream.from(0).take(10000).foldLeft(0l)(LL_L(_ + _)),
        (0 until 10000).sum
      )
    },
    test("foldLeft-scala-count-double") { implicit T =>
      equal(
        Stream.from(0.0, by = 1.0).take(10000).foldLeft(0l)(
          LD_L((z, d) => if (d.toInt % 2 == 0) z else z + 1)),
        (0.0 until 10000 by 1.0).count(_.toInt % 2 == 0)
      )
    },
    test("foldLeft-scala-plus-double") { implicit T =>
      equal(
        Stream.from(0.0, by = 1.0).take(10000).foldLeft(0.0)(DD_D(_ + _)),
        (0.0 until 10000 by 1.0).sum
      )
    },
    test("foldLeft-unison-plus") { implicit T =>
      val plusU = UnisonToScala.toUnboxed2(Builtins.Integer_add)
      val env = (new Array[U](20), new Array[B](20), StackPtr.empty, Result())
      equal(
        Stream.fromUnison(0).take(10000).foldLeft(Value(0))(plusU(env)),
        Value((0 until 10000).sum)
      )
    },
    test("++") { implicit T =>
      equal(
        (Stream.from(0).take(10000) ++ Stream.from(20000).take(5)).sumIntegers,
        (scala.Stream.from(0).take(10000) ++ scala.Stream.from(20000).take(5)).sum
      )
    },
    test("cons") { implicit T =>
      equal(
        (-10l :: Stream.from(0).take(10)).sumIntegers,
        (-10 #:: scala.Stream.from(0).take(10)).sum
      )
    },
  )
}
