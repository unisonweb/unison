package org.unisonweb

import java.nio.file.{Files, Path, Paths}

import org.unisonweb.Term.Syntax._
import org.unisonweb.Term.Term
import org.unisonweb.util.PrettyPrint.prettyTerm
import org.unisonweb.util.Sequence

object FileCompilationTests {
  import EasyTest._
  val testFiles = Paths.get("../unison-src/tests")

  val checkResultTests = Map[String, Term](
    "fib4" -> 2249999.u,
    "stream-shouldnt-damage-stack" -> ((4950.u, 9999.u)),
    "stream/iterate-increment-take-drop-reduce" ->
      scala.Stream.from(0).take(5).drop(3).sum,
    "stream/fromint64-take-map-tosequence" ->
      Term.Sequence(Sequence(
        scala.Stream.from(0)
          .take(10)
          .map(i => (i + 1l): Term).toList: _*
      )),
    "stream/iterate-increment-take-filter-reduce" ->
      scala.Stream.from(0).take(10000).filter(_ % 2 == 0).sum.u,
    "stream/fromint64-take-foldleft-plus" ->
      (0 until 10000).sum.u,
    "stream/scan-left" ->
      scala.Stream.from(1).take(10000).scanLeft(0l)(_+_).sum.u,
    "stream/cons" ->
      (3 #:: scala.Stream.from(0) take 10).scanLeft(0)(_+_).sum.u,
    "stream/append" ->
      (Stream.from(5).take(3) ++ Stream.from(100).take(10)).take(6).sum.u,
    "stream/takewhile-dropwhile-zipwith" ->
      (Stream.from(0).takeWhile(_ < 10) zip Stream.from(77).dropWhile(_ < 103))
        .map { case (x,y) => x * y }
        .sum.u,
//    "stream/flatmap" ->
//      scala.Stream.from(0)
//        .flatMap(n => 7 #:: scala.Stream.continually(1).take(n))
//        .take(10).sum.u,
  )

  def tests = suite("compilation.file")(
    checkResultTests.toList.map((checkResult _).tupled) ++
      uncheckedEvaluation: _*
  )

  def uncheckedEvaluation: Seq[Test[Unit]] = {
    import scala.collection.JavaConverters._
    Files.walk(testFiles).iterator().asScala
      .filter {
        p => p.toString.endsWith(".u") &&
          // (_:Path).toString.dropRight is very different from
          // (_:Path).dropRight
          !checkResultTests.contains(p.toString.drop(testFiles.toString.size + 1).dropRight(2))
      }
      .map(normalize)
      .toSeq
  }

  def checkResult(filePrefix: String, result: Term): Test[Unit] = {
    val filename = s"$filePrefix.u"
    val file = testFiles.resolve(filename)
    test(s"$filePrefix = ${prettyTerm(result).render(100)}") { implicit T =>
      Bootstrap.normalizedFromTextFile(file).fold(fail(_), equalShow(_, result)(prettyTerm(_).render(100)))
    }
  }

  def normalize(p: Path): Test[Unit] = {
    test(testFiles.relativize(p).toString.dropRight(2)) {
      implicit T =>
        Bootstrap.normalizedFromTextFile(p).fold(fail(_), _ => ok)
    }
  }
}
