package org.unisonweb

import org.unisonweb.EasyTest._
import org.unisonweb.util.PrettyPrint
import Term.Syntax._

object DecompileTests {
  val env0 = Environment.standard

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
      note(PrettyPrint.prettyTerm(compilation.normalize(env0)(pingpong)).render(40), includeAlways = true)
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
      note(PrettyPrint.prettyTerm(compilation.normalize(env0)(pingpong)).render(40), includeAlways = true)
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
      note(PrettyPrint.prettyTerm(compilation.normalize(env0)(pingpong)).render(40), includeAlways = true)
      ok
    }
  )
}
