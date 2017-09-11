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

  /** specialized for IntelliJ Debugger that can't do implicit parameter resolution */
  def renderTerm(term: Term) = render(term)
  def renderTermC(term: TermC) = render(term.get)
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
    case Delayed_(name, v) => s"Delayed($name)"
    case Yield_(effect) => "Yield(...)"
    case Handle_(handler, block) => "Handle(...)"
  }

  implicit def stringRender: Render[String] = (a: String) => a
}

trait Render1[-A] {
  def render(a: A): String
}

object Render1 extends Render1Instances {
  def apply[A](implicit A: Render1[A]): Render1[A] = A
  def render[A: Render1](a: A) = Render1[A].render(a)

  /** specialized for IntelliJ Debugger that can't do implicit parameter resolution */
  def renderTerm(term: Term) = render(term)
  def renderTermC(term: TermC) = render(term)
}

trait Render1Instances {
  implicit val termRender: Render1[Term] =
    a => Render1.render(a.get)

  implicit val termCRender: Render1[TermC] =
    a => Render1.render(a.get)

  implicit def abtNothingRender[F[+_], R]: Render1[ABT[F,Nothing]] = {
    case Var_(name) => s"Var($name)"
    case Abs_(_, _) => ???
    case Tm_(_) => ???
  }
  implicit def abtRender[F[+_], R](implicit F: Functor[F], R: Render1[R], R2: Render1[F[Name]]): Render1[ABT[F,R]] = {
    case Var_(name) => name.toString
    case Abs_(name, body) => s"$name" + " -> " + R.render(body)
    case Tm_(f) => R2.render(F.map(f)(r => R.render(r)))
  }

  implicit def fRender[A](implicit R: Render1[A]): Render1[Term.F[A]] = {
    case f@Lam_(body) => s"(lambda ${R.render(body)})"
    case f@Builtin_(name) => s"builtin($name)"
    case Apply_(f, args) =>
      "(apply " + R.render(f) + " " + args.map(R.render(_)).mkString(" ") + ")"

    case Num_(value) => value.toString
    case LetRec_(bindings, body) => s"(letrec " + bindings.map(R.render).mkString(" ") + " in " + R.render(body) + ")"
    case Let_(binding, body) => s"(let ${R.render(binding)} in " + R.render(body) + ")"
    case Rec_(r) => "rec\n" + R.render(r)
    case If0_(condition, ifZero, ifNonzero) => s"(if0 $condition ${R.render(ifZero)} ${R.render(ifNonzero)})"
    case Compiled_(v) => "Compiled(" + Render1.render(v.decompile) + ")"
    case Delayed_(name, v) =>
      s"Delayed($name)"
//      if (v.evaluated) {
//        compilation.render(0.0, v.value)
//      }
//      else "Delayed(...)"
    case Yield_(effect) => "Yield(...)"
    case Handle_(handler, block) => "Handle(...)"
  }

  implicit def nameRender: Render1[Name] = (a: Name) => a.toString
  implicit def stringRender: Render1[String] = (a: String) => a
}
