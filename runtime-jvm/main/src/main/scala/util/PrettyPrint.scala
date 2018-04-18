package org.unisonweb.util

import java.lang.Double.longBitsToDouble
import java.lang.Long.toUnsignedString

import org.unisonweb.{Term, U0, UnboxedType}
import org.unisonweb.Id.{Builtin, HashRef}
import org.unisonweb.Term._

sealed abstract class PrettyPrint {
  import PrettyPrint._

  def <>(p: PrettyPrint) = p match {
    case Empty => this
    case _ => Append(this, p)
  }

  def group: PrettyPrint = Group(this)

  def nest(prefix: String) = Nest(prefix, this)

  /** Given infinite horizontal space, how many characters wide would this layout be? */
  def unbrokenWidth: Int

  /** Render this doc assuming infinite available width. */
  def renderUnbroken: String = this match {
    case Empty => ""
    case Literal(s) => s
    case Append(d1, d2) => d1.renderUnbroken + d2.renderUnbroken
    case Nest(_, d) => d.renderUnbroken
    case Breakable(delim) => delim
    case Group(d) => d.renderUnbroken
  }

  def renderBroken(width: Int, newline: Boolean): String = this match {
    case Empty => ""
    case Literal(s) => s
    case Append(d1, d2) =>
      val rd1 = d1.renderBroken(width, newline)
      val lengthOfLastLine = rd1.length - (rd1.lastIndexOf("\n") + 1)
      rd1 + d2.renderBroken(width - lengthOfLastLine, lengthOfLastLine == 0)
    case Nest(prefix, d) =>
      if (newline)
        prefix + d.renderBroken(width - prefix.length, false).replaceAllLiterally("\n", "\n" + prefix)
      else d.renderBroken(width, false)
    case Breakable(_) => "\n"
    case Group(d) => d.render(width)
  }

  def render(width: Int): String =
    if (unbrokenWidth <= width) this.renderUnbroken
    else this.renderBroken(width, false)
}

object PrettyPrint {
  /** The empty document. */
  case object Empty extends PrettyPrint { def unbrokenWidth = 0 }

  /** Embed a string into a document. */
  case class Literal(get: String) extends PrettyPrint { def unbrokenWidth = get.length }

  /** Append two docs. */
  case class Append(doc1: PrettyPrint, doc2: PrettyPrint) extends PrettyPrint {
    val unbrokenWidth = doc1.unbrokenWidth + doc2.unbrokenWidth
  }

  /** If immediately preceded by a newline, indent the doc by the given element
   * otherwise ignore the `prefix` argument. */
  case class Nest(prefix: String, doc: PrettyPrint) extends PrettyPrint {
    val unbrokenWidth = doc.unbrokenWidth
  }

  /** Specify that layout may insert a line break at this point in the document.
   *  If a line break is not inserted, the given `delim` is inserted instead. */
  case class Breakable(delim: String) extends PrettyPrint { def unbrokenWidth = delim.length }

  /** Wrap this doc in a group, which constrains all layout choices in the group
   *  to be the same. */
  case class Group(doc: PrettyPrint) extends PrettyPrint {
    val unbrokenWidth = doc.unbrokenWidth
  }

  def group(doc: PrettyPrint) = Group(doc)

  def parenthesizeGroupIf(b: Boolean)(doc: PrettyPrint) = parenthesizeIf(b)(group(doc))

  def parenthesize(doc: PrettyPrint): PrettyPrint = "(" <> doc <> ")"

  def parenthesizeIf(b: Boolean)(doc: PrettyPrint) = if (b) parenthesize(doc) else doc

  implicit def lit(s: String): PrettyPrint = Literal(s)

  val softbreak = Breakable(" ")
  def softbreaks(docs: Seq[PrettyPrint]): PrettyPrint = docs.reduce(_ <> softbreak <> _)

  val semicolon = Breakable("; ")
  def semicolons(docs: Seq[PrettyPrint]): PrettyPrint = docs.reduce(_ <> semicolon <> _)

  def prettyName(name: Name) = parenthesizeIf(isOperatorName(name.toString))(name.toString)

  def unqualifiedName(name: Name): String =
    name.toString.reverse.takeWhile(_ != '.').reverse

  def isOperatorName(s: String): Boolean =
    s.forall(c => !c.isLetterOrDigit && !c.isControl && !c.isSpaceChar && !c.isWhitespace)

  def infixName(name: Name) = {
    val s = name.toString
    if (s.contains('.')) {
      val suffix = unqualifiedName(name)
      if (isOperatorName(suffix))
        suffix + "_" + s.take(s.length - suffix.length - 1)
      else
        s
    }
    else s
  }

  def prettyBinding(name: Name, term: Term): PrettyPrint = term match {
    case Lam(names, body) =>
      group(group(softbreaks((name +: names).map(prettyName))) <> " =" <> softbreak <> prettyTerm(body, 0).nest("  "))

    case _ => name.toString <> " = " <> prettyTerm(term, 0)
  }

  def prettyTerm(t: Term): PrettyPrint = prettyTerm(Term.selfToLetRec(t), 0)

  private def prettyTerm(t: Term, precedence: Int): PrettyPrint = t match {
    case Term.Unboxed(value, t) =>
      t match {
        case UnboxedType.Integer => value.toString
        case UnboxedType.Float => longBitsToDouble(value).toString
        case UnboxedType.Boolean => (value != U0).toString
        case UnboxedType.Natural => toUnsignedString(value)
      }

    case If(cond, ifZero, ifNonzero) => parenthesizeGroupIf(precedence > 0) {
      "if " <> prettyTerm(cond, 0) <> " then" <> softbreak <>
               prettyTerm(ifZero, 0).nest("  ") <> softbreak <> "else" <> softbreak <>
               prettyTerm(ifNonzero, 0).nest("  ")
    }

    case Apply(VarOrBuiltin(name), List(arg1, arg2)) if isOperatorName(unqualifiedName(name)) =>
       parenthesizeGroupIf(precedence > 5) {
        prettyTerm(arg1, 5) <> " " <> infixName(name) <> softbreak <> prettyTerm(arg2, 6).nest("  ")
    }
    case Apply(f, args) => parenthesizeGroupIf(precedence > 9) {
      prettyTerm(f, 9) <> softbreak <>
        softbreaks(args.map(arg => prettyTerm(arg, 10).nest("  ")))
    }
    case Var(name) => prettyName(name)
    case Id(Builtin(name)) => prettyName(name)
    case Id(HashRef(hash)) => ???
    case Self(name) =>
      sys.error("Self terms shouldn't exist after calling `Term.selfToLetRec`, which we do before calling this function.")
    case Lam(names, body) => parenthesizeGroupIf(precedence > 0) {
      softbreaks(names.map(name => lit(name.toString))) <> " ->" <> softbreak <>
        prettyTerm(body, 0).nest("  ")
    }
    case Let(bindings, body) => parenthesizeGroupIf(precedence > 0) {
      "let" <> softbreak <>
        semicolons(bindings.map((prettyBinding _).tupled)).nest("  ") <> semicolon <>
        prettyTerm(body, 0).nest("  ")
    }
    case LetRec(bindings, body) => parenthesizeGroupIf(precedence > 0) {
      "let rec" <> softbreak <>
        semicolons(bindings.map((prettyBinding _).tupled)).nest("  ") <> semicolon <>
        prettyTerm(body, 0).nest("  ")
    }
    case t => t.toString
  }

  // this is only used for rendering infix operators
  object VarOrBuiltin {
    def unapply(term: Term): Option[Name] = term match {
      case Var(name) => Some(name)
      case Id(Builtin(name)) => Some(name)
      case _ => None
    }
  }
}

