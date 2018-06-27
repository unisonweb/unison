package org.unisonweb
package util

import java.lang.Long.toUnsignedString
import Term.{Term, Name}
import Term.Syntax._

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
  val hashPrecision = 8

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
  def softbreaks(docs: Seq[PrettyPrint]): PrettyPrint =
    if (docs.isEmpty) Empty
    else docs.reduce(_ <> softbreak <> _)

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
    case Term.Lam(names, body) =>
      group(group(softbreaks((name +: names).map(prettyName))) <> " =" <> softbreak <> prettyTerm(body, 0).nest("  "))

    case _ => name.toString <> " = " <> prettyTerm(term, 0)
  }

  def prettyCase(c: Term.MatchCase[Term.Term]): PrettyPrint = c match {
    case Term.MatchCase(p, guard, ABT.AbsChain(names, body)) =>
      // <p> "|" guard -> <body>
      group(group(prettyPattern(p, names, 0) <>
              guard.fold[PrettyPrint]("")(g => " | " <> prettyTerm(g, 0))) <>
        " ->" <> softbreak <> prettyTerm(body, precedence = 0).nest("  "))
    case Term.MatchCase(p, guard, body) =>
      group(group(prettyPattern(p, Nil, 0) <>
              guard.fold[PrettyPrint]("")(g => " | " <> prettyTerm(g, 0))) <>
        " ->" <> softbreak <> prettyTerm(body, precedence = 0).nest("  "))
  }

  def prettyId(typeId: Id, ctorId: ConstructorId): PrettyPrint = typeId match {
    case Id.Builtin(name) => prettyName(name) <> s"#${ctorId.toInt}"
    case Id.HashRef(h) =>
      val hashString = Base58.encode(h.bytes).take(hashPrecision)
      s"#$hashString#${ctorId.toInt}"
  }

  def distributeNames(patterns: Seq[Pattern], names: List[Name]): Seq[PrettyPrint] =
    distributeSomeNames(patterns, names)._1

  def distributeSomeNames(patterns: Seq[Pattern], names: List[Name]): (Seq[PrettyPrint], List[Name]) =
    (patterns.foldLeft((Seq.empty[PrettyPrint], names)) {
      case ((prettyPrints, names), pattern) =>
        val (names1, names2) = names.splitAt(pattern.arity)
        (prettyPrints :+ prettyPattern(pattern, names1, 9), names2)
    })

  def prettyPattern(p: Pattern, names: List[Name], precedence: Int): PrettyPrint = p match {
    case Pattern.LiteralU(u, typ) =>
      prettyTerm(Term.Unboxed(u, typ), 0)
    case Pattern.Wildcard => prettyName(names.head)
    case Pattern.Uncaptured => "_"
    // ex: {Foo x y} or {x}
    case Pattern.EffectPure(p) => "{" <> prettyPattern(p, names, 0) <> "}"
    // ex: {State.set s -> k} or {Remote.at node c -> k2}
    case Pattern.EffectBind(id,cid,ps,k) =>
      val (pretties, remNames) = distributeSomeNames(ps, names)
      "{" <> softbreaks(prettyId(id,cid) +: pretties) <>
             softbreak <> "-> " <> prettyPattern(k,remNames,0) <> "}"
    case Pattern.Data(typeId, ctorId, patterns) =>
      parenthesizeGroupIf(precedence > 0) {
        softbreaks(
          prettyId(typeId, ctorId) +: distributeNames(patterns, names)
        )
      }
    case Pattern.As(p) =>
      prettyName(names.head) <> "@" <> prettyPattern(p, names.tail, 9)
    case other => other.toString
  }

  def prettyTerm(t: Term): PrettyPrint = prettyTerm(t, 0)

  def prettyUnboxed(value: U, t: UnboxedType): PrettyPrint = t match {
    case UnboxedType.Int64 =>
      val i = unboxedToInt(value)
      if (i > 0) "+" + i.toString else i.toString
    case UnboxedType.Float => unboxedToDouble(value).toString
    case UnboxedType.Boolean => unboxedToBool(value).toString
    case UnboxedType.UInt64 => toUnsignedString(unboxedToLong(value))
  }

  def prettyTerm(t: Term, precedence: Int): PrettyPrint = t match {
    case Term.Unboxed(value, t) =>
      prettyUnboxed(value, t)

    case Term.If(cond, ifZero, ifNonzero) => parenthesizeGroupIf(precedence > 0) {
      "if " <> prettyTerm(cond, 0) <> " then" <> softbreak <>
               prettyTerm(ifZero, 0).nest("  ") <> softbreak <> "else" <> softbreak <>
               prettyTerm(ifNonzero, 0).nest("  ")
    }
    case Term.Handle(handler, body) =>
      "handle " <> prettyTerm(handler, 10) <> softbreak <>
                   prettyTerm(body, 0).nest("  ")
    case Term.Request(id,cid) => prettyId(id, cid)
    case Term.Apply(VarOrBuiltin(name), List(arg1, arg2)) if isOperatorName(unqualifiedName(name)) =>
       parenthesizeGroupIf(precedence > 5) {
        prettyTerm(arg1, 5) <> " " <> infixName(name) <> softbreak <> prettyTerm(arg2, 6).nest("  ")
    }
    case Term.Apply(f, args) => parenthesizeGroupIf(precedence > 9) {
      prettyTerm(f, 9) <> softbreak <>
        softbreaks(args.map(arg => prettyTerm(arg, 10).nest("  ")))
    }
    case Term.Var(name) => prettyName(name)
    case Term.Id(Id.Builtin(name)) => prettyName(name)
    case Term.Id(Id.HashRef(hash)) => ???
    case Term.Lam(names, body) => parenthesizeGroupIf(precedence > 0) {
      group(softbreaks(names.map(name => lit(name.toString)))) <> " ->" <> softbreak <>
        prettyTerm(body, 0).nest("  ")
    }
    case Term.Let(bindings, body) => parenthesizeGroupIf(precedence > 0) {
      "let" <> softbreak <>
        semicolons(bindings.map((prettyBinding _).tupled)).nest("  ") <> semicolon <>
        prettyTerm(body, 0).nest("  ")
    }
    case Term.LetRec(bindings, body) => parenthesizeGroupIf(precedence > 0) {
      "let rec" <> softbreak <>
        semicolons(bindings.map((prettyBinding _).tupled)).nest("  ") <> semicolon <>
        prettyTerm(body, 0).nest("  ")
    }
    case Term.Match(scrutinee, cases) => parenthesizeGroupIf(precedence > 0) {
      "case " <> prettyTerm(scrutinee, 0) <> " of" <> softbreak <>
        semicolons(cases.map(prettyCase)).nest("  ")
    }
    case Term.Constructor(id,cid) => prettyId(id,cid)
    case Term.Compiled(Value.Data(typeId, ctorId, fields)) =>
      prettyTerm(
        Term.Var(prettyId(typeId, ctorId).renderUnbroken)(
          fields.map(_.decompile):_*), precedence)
    case Term.Text(txt) => '"' + Text.toString(txt) + '"'
    case t => t.toString
  }

  // this is only used for rendering infix operators
  object VarOrBuiltin {
    def unapply(term: Term): Option[Name] = term match {
      case Term.Var(name) => Some(name)
      case Term.Id(Id.Builtin(name)) => Some(name)
      case _ => None
    }
  }
}

