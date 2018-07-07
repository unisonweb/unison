package org.unisonweb.util

import org.unisonweb.EasyTest._
import org.unisonweb.Term.Term
import org.unisonweb._
import org.unisonweb.util.Unboxed.F1.{D_B, L_B, L_L, L_P}
import org.unisonweb.util.Unboxed.F2.{DD_D, LD_L, LL_L}
import org.unisonweb.util.Unboxed.Unboxed

object StreamTests {
  implicit class SourceRunner(private val s: String) extends AnyVal {
    def runPipes(implicit t: EasyTest.Env): Term =
      Bootstrap.normalizedFromText(
        s"""a |> f = f a
           |$s
          """.stripMargin
      ).fold(fail(_), identity)
  }

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
          Stream.from(0).take(10000).map(L_L(_ + 1)).toSequence.toList,
          (0 until 10000).map(_ + 1).toList
        )
      },
      test("flatMap 0") { implicit T =>
        equal(
          Stream.from(1).take(100)
            .flatMap(L_P(n => Stream.constant(n).take(n))).toSequence.toList,
          scala.Stream.from(1).take(100)
            .flatMap(n => scala.Stream.continually(n).take(n)).toList
        )
      },
      test("flatMap 1") { implicit T =>
        equal(
          Stream.from(1).take(100)
            .flatMap(L_P(n => Stream.constant(n).take(n))).toSequence.toList,
          scala.Stream.from(1).take(100)
            .flatMap(n => scala.Stream.continually(n).take(n)).toList
        )
      },
      test("flatMap inf-fin-take") { implicit T =>
        equal(
          Stream.from(0).flatMap(L_P[Stream[Unboxed[Long]]](n => Stream.singleton(n))).take(3).toSequence.toList,
          scala.Stream.from(0).flatMap(n => scala.Stream(n)).take(3).map(_.toLong).toList
        )
      },
      test("flatMap inf-inf-take") { implicit T =>
        equal(
          Stream.from(0).flatMap(L_P[Stream[Unboxed[Long]]](n => Stream.constant(n))).take(3).toSequence.toList,
          scala.Stream.from(0).flatMap(n => scala.Stream.continually(n)).take(3).map(_.toLong).toList
        )
      },
      test("flatMap inf-consinf-take") { implicit T =>
        equal(
          Stream.from(0).flatMap(L_P[Stream[Unboxed[Long]]](n => 7l :: Stream.constant(n))).take(5).toSequence.toList,
          scala.Stream.from(0).flatMap(n => 7 #:: scala.Stream.continually(n)).take(5).map(_.toLong).toList
        )
      },
      test("unfold") { implicit T =>
        equal(
          // Stream.take 5 (Stream.unfold (b -> if b < 1 then Some (b + 1, b / 2) else None) -2)
          Stream.unfold[Option[(Long,Long)],(Long,Long),Unboxed[Long],Unboxed[Long],Long](-2)(
            L_P(b => if (b < 1) Some((b + 1l, b / 2l)) else None)
          ).take(5).toSequence.toList,
          List(-2/2, -1/2, 0/2)
        )
      },
      test("filter") { implicit T =>
        equal(
          Stream.from(0).take(10000).filter(L_B(_ % 2 == 0)).toSequence.toList,
          (0 until 10000).filter(_ % 2 == 0).toList
        )
      },
      test("takeWhile") { implicit T =>
        equal(
          Stream.from(0).take(100).takeWhile(L_B(_ < 50)).toSequence.toList,
          (0 until 100).takeWhile(_ < 50).toList
        )
        equal(
          Stream.from(0.0, by = 1.0).take(100).takeWhile(D_B(_ < 50.0)).toSequence.toList,
          (0 until 100).takeWhile(_ < 50).toList
        )
      },
      test("dropWhile") { implicit T =>
        equal(
          Stream.from(0).take(100).dropWhile(L_B(_ < 50)).toSequence.toList,
          (0 until 100).dropWhile(_ < 50).toList
        )
      },
      test("zipWith") { implicit T =>
        val s1 = Stream.from(0)
        val s2 = scala.collection.immutable.Stream.from(0)
        equal(
          s1.zipWith(s1.drop(1))(LL_L(_ * _)).take(100).toSequence.toList,
          s2.zip(s2.drop(1)).map { case (a,b) => a * b }.take(100).toList
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
          (Stream.from(0).take(10000) ++ Stream.from(20000).take(5))
            .toSequence.toList,
          (scala.Stream.from(0).take(10000) ++ scala.Stream.from(20000).take(5))
            .toList
        )
        equal(
          (Stream.from(0).drop(10000).take(10000) ++ Stream.from(20000).take(5))
            .toSequence.toList,
          (scala.Stream.from(0).drop(10000).take(10000) ++
            scala.Stream.from(20000).take(5)).toList
        )
      },
      test("cons") { implicit T =>
        equal(
          (-10l :: Stream.from(0).take(10)).toSequence.toList,
          (-10 #:: scala.Stream.from(0).take(10)).toList
        )
      },
      test("iterate-from0") { implicit T =>
        equal(
          Stream.iterate(0l)(L_L(_ + 1)).take(10).toSequence.toList,
          (scala.Stream.from(0).take(10)).toList
        )
      }
    )
  )
}
