package org.unisonweb

import java.io.File
import java.nio.file.{Files, Path}

import org.unisonweb.Term.Syntax._
import org.unisonweb.Term.Term
import org.unisonweb.util.PrettyPrint.prettyTerm

object FileCompilationTests {
  import EasyTest._
  val testFiles = new File("../unison-src/tests").toPath

  val checkResultTests = Map[String, Term](
    "fib4" -> 2249999.u,
    "stream-shouldnt-damage-stack" -> ((4950.u, 9999.u)),
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
          !checkResultTests.contains(p.getFileName.toString.dropRight(2))
      }
      .map(normalize)
      .toSeq
  }

  def checkResult(filePrefix: String, result: Term): Test[Unit] = {
    val filename = s"$filePrefix.u"
    val file = testFiles.resolve(filename)
    test(s"$filePrefix = ${prettyTerm(result).render(100)}") { implicit T =>
      Bootstrap.normalizedFromTextFile(file.toFile).fold(fail(_), equal(_, result))
    }
  }

  def normalize(p: Path): Test[Unit] = {
    test(testFiles.relativize(p).toString.dropRight(2)) {
      implicit T =>
        Bootstrap.normalizedFromTextFile(p.toFile).fold(fail(_), _ => ok)
    }
  }
}
