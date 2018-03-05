package org.unisonweb.util

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

  import org.unisonweb.Term._

  def prettyName(name: Name) = parenthesizeIf(isOperatorName(name))(name.toString)

  def prettyBinding(name: Name, term: Term): PrettyPrint = term match {
    case Lam(names, body) =>
      group(group(softbreaks((name +: names).map(prettyName))) <> " =" <> softbreak <> prettyTerm(body, 0).nest("  "))

    case _ => name.toString <> " = " <> prettyTerm(term, 0)
  }

  def prettyTerm(t: Term, precedence: Int = 0): PrettyPrint = t match {
    case Num(value) =>
      if (value == value.toLong)
        value.toLong.toString
      else value.toString

    case If0(cond, ifZero, ifNonzero) => parenthesizeGroupIf(precedence > 0) {
      "if " <> prettyTerm(cond, 0) <> " == 0 then" <> softbreak <>
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
    case Builtin(name) => prettyName(name)
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

    //implicit def fRender[A](implicit R: Render[A]): Render[Term.F[A]] = {
    //  case Compiled_(v) => "Compiled {" + renderIndent(v.decompile) + "}"
    //  case Delayed_(name, v) => s"Delayed($name)"
    //  case Yield_(effect) => "Yield(...)"
    //  case Handle_(handler, block) => "Handle(...)"
    //}
  }

  object VarOrBuiltin {
    def unapply(term: Term): Option[Name] = term match {
      case Var(name) => Some(name)
      case Builtin(name) => Some(name)
      case _ => None
    }
  }

  def isOperatorName(name: Name): Boolean =
    name.toString.forall(c => !c.isLetterOrDigit && !c.isControl && !c.isSpaceChar && !c.isWhitespace)

}

