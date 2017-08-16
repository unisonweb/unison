package org.unisonweb

package object codegeneration {
  val maxInlineStack = 4
  val maxInlineArgs = 3
  val maxInlineTC = 2

  def applySignature(i: Int): String =
    "def apply(rec: Lambda, " + (0 until i).commas(i => s"x$i: D, x${i}b: V") + commaIf(i) + "r: R): D"

  def applyNSignature: String =
    "def apply(rec: Lambda, xs: Array[Slot], r: R): D"

  def indent(level: Int, lines: Seq[String]): String =
    lines.flatMap(multilines =>
      multilines.split('\n').map(
        line => ("  " * level) ++ line
      )
    ).mkString("\n")

  def lines(s: String*) = s.mkString("\n")
  def braced(s: String) = s.b
  def commaIf(i: Int) = if (i > 0) ", " else ""

  // tailEval(4, "foo") = "foo(rec, x0, x0b, x1, x1b, x2, x2b, x3, x3b, r)"
  def tailEval(i: Int, expr: String): String =
   expr + "(rec, "+xArgs(i)+commaIf(i)+ "r)"

  // tailEvalN(2, "foo") = "foo(rec, xs(0).unboxed, xs(0).boxed, xs(1).unboxed, xs(1).boxed, r)"
  def tailEvalN(i: Int, expr: String): String =
   expr + "(rec, "+xsArgs(i)+commaIf(i)+ "r)"

  def catchTC(expr: String) =
    "try { " + expr + " } catch { case e: TC => loop(e,r) }"

  // def eval(i: Int, expr: String) =
  //  "try " + tailEval(i, expr) + " catch { case e: TC => loop(e,r) }"

  // def evalN(expr: String) =
  //  "try " + expr + "(rec, xs, r) catch { case e: TC => loop(e,r) }"

  private // don't ask why it's called this
  def indentify(level: Int, lines: String): String =
    indent(level, lines :: Nil)

  implicit class Codegen1(val r: Seq[Int]) extends AnyVal {
    def each(f: Int => String): String = r map f mkString "\n"
    def semis(f: Int => String): String = r map f mkString "; "
    def commas(f: Int => String): String = r map f mkString ", "
  }

  implicit class Codegen2(val s: String) extends AnyVal {
    def indentBy(by: Int) = indentify(by, s)
    def indent = indentify(1, s)
    def bracedBy(by: Int) = "{" <> s.indentBy(by + 1) <> "}".indentBy(by) // K&R FTW
    def b = s.bracedBy(0)
    def <>(s2: String) = s + "\n" + s2
    def nl(s2: String) = s <> s2
  }

  def slot(i: Int) = s"Slot(x$i, x${i}b)"

  def xArgs(count: Int): String = xArgs(0, count)
  private def xArgs(start: Int, count: Int): String =
   (start until (start+count)).commas(i => s"x$i, x${i}b")
  def xsArgs(count: Int): String = xsArgs(0, count)
  private def xsArgs(start: Int, count: Int): String =
   start until (start + count) commas (i => s"xs($i).unboxed, xs($i).boxed")
}
