package org.unisonweb

import org.unisonweb.ABT._
import org.unisonweb.Term.{F, Term}
import org.unisonweb.Term.F._
import org.unisonweb.util.Functor

trait Render[-A] {
  def render(a: A): String
}

object Render extends RenderInstances {
  def apply[A](implicit A: Render[A]): Render[A] = A
  def render[A: Render](a: A) = Render[A].render(a)
  def renderTerm(a: Term) = render(a.get)
}

trait RenderInstances {
  implicit val termRender: Render[Term] =
    a => Render[ABT[F,AnnotatedTerm[F,Set[Name]]]].render(a.get)

  implicit def abtNothingRender[F[+_], R]: Render[ABT[F,Nothing]] = {
    case Var_(name) => name
    case Abs_(_, _) => ???
    case Tm_(_) => ???
  }

  def indent(s: String): String = "  " + s.replace("\n", "\n  ")
  def renderIndent[A](a: A)(implicit R: Render[A]): String = indent(R.render(a))

  implicit def fRender[A](implicit R: Render[A]): Render[Term.F[A]] = {
    case f@Lam_(body) => s"lambda ${R.render(body)}"
    case f@Builtin_(name) => name
    case Apply_(f, args) => args.map(R.render).mkString(s"($f ", " ", ")")
    case Num_(value) => value.toString
    case LetRec_(bindings, body) => s"let rec" + bindings.map(renderIndent[A]).mkString("\n", "\n", "\n") + "in\n" + renderIndent(body)
    case Let_(binding, body) => s"let ${R.render(binding)}\nin\n" + renderIndent(body)
    case Rec_(r) => "rec " + R.render(r)
    case If0_(condition, ifZero, ifNonzero) => s"ifZero $condition\nthen\n${renderIndent(ifZero)}\nelse\n${renderIndent(ifNonzero)}"
    case Compiled_(v) => "Compiled {" + renderIndent(v.decompile) + "}"
    case Yield_(effect) => "Yield(...)"
    case Handle_(handler, block) => "Handle(...)"
  }

  implicit def setRender[A]: Render[Set[A]] = (a: Set[A]) => a.toString

  implicit def stringRender: Render[String] = (a: String) => a

  implicit def abtRender[F[+_], R](implicit F: Functor[F], R: Render[R], R2: Render[F[String]]): Render[ABT[F,R]] = {
    case Var_(name) => name
    case Abs_(name, body) => name + " ->\n" + renderIndent(body)
    case Tm_(f) => R2.render(F.map(f)(r => R.render(r)))
  }

}
