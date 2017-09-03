package org.unisonweb

import org.unisonweb.ABT._
import org.unisonweb.Term.Term
import org.unisonweb.Term.F._
import org.unisonweb.util.Functor
import org.unisonweb.compilation.TermC

trait Render[-A] {
  def render(a: A): String
}

object Render extends RenderInstances {
  def apply[A](implicit A: Render[A]): Render[A] = A
  def render[A: Render](a: A) = Render[A].render(a)
}

trait RenderInstances {
  implicit val termRender: Render[Term] =
    a => Render.render(a.get)

  implicit val termCRender: Render[TermC] =
    a => a.annotation.toString + ": " + Render.render(a.get)

  implicit def abtNothingRender[F[+_], R]: Render[ABT[F,Nothing]] = {
    case Var_(name) => s"Var($name)"
    case Abs_(_, _) => ???
    case Tm_(_) => ???
  }
  implicit def abtRender[F[+_], R](implicit F: Functor[F], R: Render[R], R2: Render[F[String]]): Render[ABT[F,R]] = {
    case Var_(name) => s"var $name"
    case Abs_(name, body) => s"abs $name" + " ->\n" + R.render(body)
    case Tm_(f) => R2.render(F.map(f)(r => R.render(r)))
  }

  def indent(s: String): String = "  " + s.replace("\n", "\n  ")
  def renderIndent[A](a: A)(implicit R: Render[A]): String = indent(R.render(a))

  implicit def fRender[A](implicit R: Render[A]): Render[Term.F[A]] = {
    case f@Lam_(body) => s"lambda\n${renderIndent(body)}"
    case f@Builtin_(name) => s"builtin($name)"
    case Apply_(f, args) =>
      ("apply\n"
        + indent("fn =\n" + renderIndent(f)) + "\n"
        + indent(args.zipWithIndex.map{ case (a, i) => s"arg$i =\n${renderIndent(a)}" }.mkString("\n"))
        )
    case Num_(value) => value.toString
    case LetRec_(bindings, body) => s"letrec" + bindings.map(renderIndent[A]).mkString("\n", "\n", "\n") + "in\n" + renderIndent(body)
    case Let_(binding, body) => s"let ${R.render(binding)}\nin\n" + renderIndent(body)
    case Rec_(r) => "rec\n" + renderIndent(r)
    case If0_(condition, ifZero, ifNonzero) => s"ifZero $condition\nthen\n${renderIndent(ifZero)}\nelse\n${renderIndent(ifNonzero)}"
    case Compiled_(v) => "Compiled {" + renderIndent(v.decompile) + "}"
    case Delayed_(v) => "Delayed { ... }"
    case Yield_(effect) => "Yield(...)"
    case Handle_(handler, block) => "Handle(...)"
  }

  implicit def stringRender: Render[String] = (a: String) => a
}
