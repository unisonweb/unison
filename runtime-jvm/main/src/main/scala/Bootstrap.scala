package org.unisonweb

import java.io.File

import org.unisonweb.ABT.Name
import org.unisonweb.Term.Term
import org.unisonweb.BuiltinTypes._
import org.unisonweb.compilation._
import org.unisonweb.util._

object Bootstrap {
  def main(args: Array[String]): Unit = {

    val fileName = args match {
      case Array(s) => s
      case _ =>
        println("usage: Bootstrap <file.ub>")
        sys.exit(1)
    }

    println {
      PrettyPrint.prettyTerm(normalizedFromBinaryFile(fileName)).render(80)
    }
  }

  def normalizedFromBinaryFile(fileName: String): Term =
    normalizedFromBinarySource(Source.fromFile(fileName))

  def normalizedFromBinarySource(src: Source): Term = {
    fromBinarySource(src, normalize(_)(_))
  }

  def fromBinarySource[A](src: Source, f: (Environment, Term) => A): A = {
    import org.unisonweb.Codecs.{decodeConstructorArities, termDecoder}

    val data = decodeConstructorArities(src)
    val effects = decodeConstructorArities(src)

    val term = termDecoder(src)

    val datas = data.flatMap { case (id, arities) =>
      arities.zipWithIndex.map {
        case (arity, cid) =>
          dataConstructor(id, ConstructorId(cid),
                          Range(0,arity).map(x => Name("x" + x)):_*)
      }
    }.toMap

    val effectss = effects.flatMap { case (id, arities) =>
      arities.zipWithIndex.map {
        case (arity, cid) =>
          effectRequest(id, ConstructorId(cid),
                        Range(0,arity).map(x => Name("x" + x)):_*)
      }
    }.toMap

    val env =
      Environment.standard.copy(
        dataConstructors = Environment.standard.dataConstructors ++ datas,
        effects = Environment.standard.effects ++ effectss
      )

    f(env, term)
  }

  val PROJECT_ROOT_ENV_NAME = "UNISON_PROJECT_ROOT"
  val UNISON_PROJECT_ROOT =
    new File(Option(System.getProperty(PROJECT_ROOT_ENV_NAME))
               .orElse(Option(System.getenv(PROJECT_ROOT_ENV_NAME)))
               .getOrElse(".."))

  def normalizedFromTextFile(u: File,
                             stackDir: File = UNISON_PROJECT_ROOT
                            ): Either[String,Term] = {
    if (! new File(UNISON_PROJECT_ROOT, "stack.yaml").isFile) Left {
      import java.nio.file.FileSystems
      import java.nio.file.Path

      val path: Path = FileSystems.getDefault.getPath(".").toAbsolutePath
      s"""Expected to find `stack.yaml` in `$UNISON_PROJECT_ROOT` to `stack exec bootstrap`,
         |but didn't. Specify the correct directory by setting a Java system property or
         |system environment variable called `$PROJECT_ROOT_ENV_NAME`.
         |Current directory is: $path"""
        .stripMargin
    }
    else {
      import sys.process._
      val ub = File.createTempFile(u.getName, ".ub")
      val stderrBuffer = new StringBuffer
      val log = new ProcessLogger {
        def buffer[T](f: => T): T = f
        def out(s: => String): Unit = ()
        def err(s: => String): Unit = {
          stderrBuffer.append(s)
          stderrBuffer.append("\n")
          ()
        }
      }
      val result =
        Process(Seq("stack", "build"), stackDir) #&&
          Process(Seq("stack", "exec", "bootstrap", u.toString, ub.toString)) ! log
      if (result > 0) Left(stderrBuffer.toString)
      else Right(normalizedFromBinaryFile(ub.toString))
    }
  }
}
