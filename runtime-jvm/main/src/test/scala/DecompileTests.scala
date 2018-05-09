package org.unisonweb

import org.unisonweb.EasyTest._
import org.unisonweb.util.PrettyPrint
import Term.Syntax._
import Term.Term

object DecompileTests {
  val env0 = Environment.standard

  def roundTrip(t: Term) = {
    val bytes = Codecs.encodeTerm(t)
    println(Codecs.prettyEncoding(bytes))
    Codecs.decodeTerm(bytes)
  }

  val tests = suite("decompile") (
    test("ex1") { implicit T =>
      val pingpong =
        Term.Let("k" -> 79) {
          Term.LetRec(
            "ping" -> Term.Lam("x")(Term.Var("pong")(Term.Var("x"))),
            "pong" -> Term.Lam("x")(Term.Var("k"))) {
              Term.Var("pong")
            }
        }
      val result = roundTrip { compilation.normalize(env0)(pingpong, fullyDecompile = false) }
      note(PrettyPrint.prettyTerm(Term.fullyDecompile(result)).render(40), includeAlways = true)
      ok
    },
    test("ex2") { implicit T =>
      val pingpong =
        Term.Let("id" -> Term.Lam("x")(Term.Var("x"))) {
          Term.LetRec(
            "ping" -> Term.Lam("x")(Term.Var("pong")(Term.Var("id")(Term.Var("x")))),
            "pong" -> Term.Lam("x")(Term.Var("ping")(Term.Var("x")))) {
              Term.Var("ping")
            }
        }
      val result = roundTrip { compilation.normalize(env0)(pingpong, fullyDecompile = false) }
      note(PrettyPrint.prettyTerm(Term.fullyDecompile(result)).render(40), includeAlways = true)
      ok
    },
    test("ex3") { implicit T =>
      val pingpong =
        Term.LetRec(
          "foo" -> Term.Lam("x")(Term.Var("pang")(Term.Var("x"))),
          "pang" -> Term.Lam("x")(Term.Var("ping")(Term.Var("x"))),
          "ping" -> Term.Lam("x")(Term.Var("pong")(Term.Var("x"))),
          "pong" -> Term.Lam("x")(Term.Var("ping")(Term.Var("pang")(Term.Var("x"))))) {
            Term.Var("ping")
          }
      val result0 = { compilation.normalize(env0)(pingpong, fullyDecompile = false) }
      note(PrettyPrint.prettyTerm(Term.fullyDecompile(result0)).render(40), includeAlways = true)
      val result = roundTrip { result0 }
      ok
    }
  )
}
