package org.unisonweb
package util

import java.lang.Double.longBitsToDouble
import java.lang.Long.toUnsignedString

import org.unisonweb.{Term, U0, UnboxedType}
import org.unisonweb.Term.{Id => _, _}
import org.unisonweb.Id

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

  def prettyName(name: Name) = parenthesizeIf(isOperatorName(name))(name.toString)

  def prettyBinding(name: Name, term: Term): PrettyPrint = term match {
    case Lam(names, body) =>
      group(group(softbreaks((name +: names).map(prettyName))) <> " =" <> softbreak <> prettyTerm(body, 0).nest("  "))

    case _ => name.toString <> " = " <> prettyTerm(term, 0)
  }

  def prettyCase(c: MatchCase[Term.Term]): PrettyPrint = c match {
    case MatchCase(p, guard, ABT.AbsChain(names, body)) =>
      // <p> "|" guard -> <body>
      group(group(prettyPattern(p, names, 0) <>
              guard.fold[PrettyPrint]("")(g => " | " <> prettyTerm(g, 0))) <>
        " ->" <> softbreak <> prettyTerm(body, precedence = 0).nest("  "))
  }

  def prettyId(typeId: Id, ctorId: ConstructorId): PrettyPrint = typeId match {
    case Id.Builtin(name) => prettyName(name) <> s"<${ctorId.toInt}>"
    case Id.HashRef(h) => "#" <> h.bytes.map(b => b.formatted("%02x")).toList.mkString
  }

  def prettyPattern(p: Pattern, names: List[Name], precedence: Int): PrettyPrint = p match {
    case Pattern.LiteralU(u, typ) =>
      prettyTerm(Term.Unboxed(u, typ), 0)
    case Pattern.Wildcard => prettyName(names.head)
    case Pattern.Uncaptured => "_"
    case Pattern.Data(typeId, ctorId, patterns) =>
      parenthesizeGroupIf(precedence > 0) {
        softbreaks(
          prettyId(typeId, ctorId) +:
            (patterns.foldLeft((Seq.empty[PrettyPrint], names)) {
              case ((prettyPrints, names), pattern) =>
                val (names1, names2) = names.splitAt(pattern.arity)
                (prettyPrints :+ prettyPattern(pattern, names1, 9), names2)
            })._1
        )
      }
    case Pattern.As(p) =>
      prettyName(names.head) <> "@" <> prettyPattern(p, names.tail, 9)
    case other => other.toString
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

    case Apply(VarOrBuiltin(name), List(arg1, arg2)) if isOperatorName(name) =>
       parenthesizeGroupIf(precedence > 5) {
        prettyTerm(arg1, 5) <> " " <> name.toString <> softbreak <> prettyTerm(arg2, 6).nest("  ")
    }
    case Apply(f, args) => parenthesizeGroupIf(precedence > 9) {
      prettyTerm(f, 9) <> softbreak <>
        softbreaks(args.map(arg => prettyTerm(arg, 10).nest("  ")))
    }
    case Var(name) => prettyName(name)
    case Term.Id(Id.Builtin(name)) => prettyName(name)
    case Term.Id(Id.HashRef(hash)) => ???
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
    case Match(scrutinee, cases) => parenthesizeGroupIf(precedence > 0) {
      "case " <> prettyTerm(scrutinee, 0) <> " of" <> softbreak <>
        semicolons(cases.map(prettyCase)).nest("  ")
    }
    case Term.Compiled(Value.Data(typeId, ctorId, fields)) =>
      prettyTerm(Var(prettyId(typeId, ctorId).renderUnbroken)(fields.map(_.decompile):_*), precedence)
    case t => t.toString
  }

  // this is only used for rendering infix operators
  object VarOrBuiltin {
    def unapply(term: Term): Option[Name] = term match {
      case Var(name) => Some(name)
      case Term.Id(Id.Builtin(name)) => Some(name)
      case _ => None
    }
  }

  def isOperatorName(name: Name): Boolean =
    name.toString.forall(c => !c.isLetterOrDigit && !c.isControl && !c.isSpaceChar && !c.isWhitespace)

}

