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

  def renderBroken(width: Int): String = this match {
    case Empty => ""
    case Literal(s) => s
    case Append(d1, d2) =>
      val rd1 = d1.renderBroken(width)
      val lengthOfLastLine = rd1.length - (rd1.lastIndexOf("\n") + 1)
      d2.renderBroken(width - lengthOfLastLine)
    case Nest(prefix, d) =>
      prefix + d.renderBroken(width - prefix.length).replace("\n", "\n" + prefix)
    case Breakable(delim) => ""
    case Group(d) => d.render(width)
  }

  def render(width: Int): String =
    if (unbrokenWidth <= width) this.renderUnbroken
    else this.renderBroken(width)
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

  implicit def lit(s: String): PrettyPrint = Literal(s)

  val softbreak = Breakable(" ")

  import org.unisonweb.Term._

  def prettyTerm(t: Term, prec: Int): PrettyPrint = t match {
    case Num(value) => value.toString
    case If0(cond, ifZero, ifNonzero) => group {
      "if " <> prettyTerm(cond, 0) <> " == 0 then" <> softbreak <>
               prettyTerm(ifZero, 0).nest("  ") <> softbreak <> "else" <> softbreak <>
               prettyTerm(ifNonzero, 0).nest("  ")
    }
    case Apply(f, args) => group {
      prettyTerm(f, 9) <> softbreak <> ???
        // args.map(_.nest("  "))
    }
  }

  //implicit def fRender[A](implicit R: Render[A]): Render[Term.F[A]] = {
  //  case f@Lam_(body) => s"lambda\n${renderIndent(body)}"
  //  case f@Builtin_(name) => s"builtin($name)"
  //  case Apply_(f, args) =>
  //    ("apply\n"
  //      + indent("fn =\n" + renderIndent(f)) + "\n"
  //      + indent(args.zipWithIndex.map{ case (a, i) => s"arg$i =\n${renderIndent(a)}" }.mkString("\n"))
  //      )
  //  case Num_(value) => value.toString
  //  case LetRec_(bindings, body) => s"letrec" + bindings.map(renderIndent[A]).mkString("\n", "\n", "\n") + "in\n" + renderIndent(body)
  //  case Let_(binding, body) => s"let ${R.render(binding)}\nin\n" + renderIndent(body)
  //  case Rec_(r) => "rec\n" + renderIndent(r)
  //  case If0_(condition, ifZero, ifNonzero) => s"ifZero $condition\nthen\n${renderIndent(ifZero)}\nelse\n${renderIndent(ifNonzero)}"
  //  case Compiled_(v) => "Compiled {" + renderIndent(v.decompile) + "}"
  //  case Delayed_(name, v) => s"Delayed($name)"
  //  case Yield_(effect) => "Yield(...)"
  //  case Handle_(handler, block) => "Handle(...)"
  //}

}

