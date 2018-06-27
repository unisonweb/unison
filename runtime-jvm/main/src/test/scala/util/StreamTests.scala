package org.unisonweb.util

import org.unisonweb.EasyTest._
import org.unisonweb._
import org.unisonweb.compilation._
import org.unisonweb.util.Unboxed.Unboxed
import org.unisonweb.util.Unboxed.F1.{L_B, D_B, L_L}
import org.unisonweb.util.Unboxed.F2.{LL_L,DD_D,LD_L}

object StreamTests {
  val tests = suite("stream")(
    suite("native")(
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
          (0 until 10000).filter(_ % 2 == 0).sum
        )
      },
      test("takeWhile") { implicit T =>
        equal(
          Stream.from(0).take(100).takeWhile(L_B(_ < 50)).sumIntegers,
          (0 until 100).takeWhile(_ < 50).sum
        )
        equal(
          Stream.from(0.0, by = 1.0).take(100).takeWhile(D_B(_ < 50.0)).sumFloats,
          (0 until 100).takeWhile(_ < 50).sum
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
      test("foldLeft0 (+) long") { implicit T =>
        equal(
          Stream.from(0).take(10000).foldLeft0(U0, null:Unboxed[Long])(
            LL_L(_ + _))((u,_) => u),
          (0 until 10000).sum
        )
      },
      test("foldLeft (+) long") { implicit T =>
        equal(
          Stream.from(0).take(10000).foldLeft(0l)(LL_L(_ + _)),
          (0 until 10000).sum
        )
      },
      test("foldLeft count even doubles") { implicit T =>
        equal(
          Stream.from(0.0, by = 1.0).take(10000).foldLeft(0l)(
            LD_L((z, d) => if (d.toInt % 2 == 0) z else z + 1)),
          (0 until 10000).count(_ % 2 == 0)
        )
      },
      test("foldLeft (+) double") { implicit T =>
        equal(
          Stream.from(0.0, by = 1.0).take(10000).foldLeft(0.0)(DD_D(_ + _)),
          (0 until 10000).sum
        )
      },
      test("scanLeft0 (+) long") { implicit T =>
        equal(
          Stream.from(7).take(10).scanLeft0(longToUnboxed(-3), null: Unboxed[Long])(LL_L(_+_)).sumIntegers,
          scala.Stream.from(7).take(10).scanLeft(-3)(_+_).sum
        )
      },
      test("scanLeft (+) long") { implicit T =>
        equal(
          Stream.from(7).take(10).scanLeft(-3l)(LL_L(_+_)).sumIntegers,
          scala.Stream.from(7).take(10).scanLeft(-3)(_+_).sum
        )
      },
      test("++") { implicit T =>
        equal(
          (Stream.from(0).take(10000) ++ Stream.from(20000).take(5)).sumIntegers,
          (scala.Stream.from(0).take(10000) ++ scala.Stream.from(20000).take(5)).sum
        )
        equal(
          (Stream.from(0).drop(10000).take(10000) ++ Stream.from(20000).take(5)).sumIntegers,
          (scala.Stream.from(0).drop(10000).take(10000) ++ scala.Stream.from(20000).take(5)).sum
        )
      },
      test("cons") { implicit T =>
        equal(
          (-10l :: Stream.from(0).take(10)).sumIntegers,
          (-10 #:: scala.Stream.from(0).take(10)).sum
        )
      },
      test("iterate-from0") { implicit T =>
        equal(
          Stream.iterate(0l)(L_L(_ + 1)).take(10).sumIntegers,
          (scala.Stream.from(0).take(10)).sum
        )
      }
    ),
    {
      def env =
        (new Array[U](20), new Array[B](20), StackPtr.empty, Result())
      val incU = UnisonToScala.toUnboxed1(Builtins.Int64_inc)
      val plusU = UnisonToScala.toUnboxed2(Builtins.Int64_add)

      suite("unison") (
        test("take/drop") { implicit T =>
          equal(
            Stream.iterate(0)(incU(env)).take(5).drop(3).reduce(0)(plusU(env)),
            scala.Stream.from(0).take(5).drop(3).sum
          )
        },
        test("map") { implicit T =>
          equal[Int](
            Stream.iterate(0)(incU(env)).take(10000).map(incU(env)).reduce(0)(plusU(env)),
            scala.Stream.from(0).take(10000).map(_ + 1).sum
          )
        },
        test("filter") { implicit T =>
          equal[Int](
            Stream.iterate(0)(incU(env)).take(10000).map(incU(env)).reduce(0)(plusU(env)),
            scala.Stream.from(0).take(10000).map(_ + 1).sum
          )
        },
        test("foldLeft Int64_add") { implicit T =>
          val plusU = UnisonToScala.toUnboxed2(Builtins.Int64_add)
          equal(
            Stream.fromUnison(0).take(10000).foldLeft(Value(0))(plusU(env)),
            Value((0 until 10000).sum)
          )
        },
        test("scanLeft Int64_add") { implicit T =>
          val int64add = UnisonToScala.toUnboxed2(Builtins.Int64_add)(env)
          equal(
            Stream.fromUnison(1).take(10000).scanLeft(Value(0))(int64add).reduce(Value(0))(int64add),
            Value(scala.Stream.from(1).take(10000).scanLeft(0l)(_+_).sum)
          )
        },
        test("iterate Int64_inc, reduce Int64_add") { implicit T =>
          val incU = UnisonToScala.toUnboxed1(Builtins.Int64_inc)
          val plusU = UnisonToScala.toUnboxed2(Builtins.Int64_add)
          equal[Value](
            Stream.iterate(0l)(incU(env)).take(10).reduce(zero = Value(0))(plusU(env)),
            Value((scala.Stream.from(0).take(10)).sum)
          )
        }
      )
    }
  )
}
