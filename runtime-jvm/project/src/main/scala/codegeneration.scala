package org.unisonweb

package object codegeneration {

  val maxInlineArity = 4

  def indent(level: Int, lines: Seq[String]): String =
    lines.flatMap(multilines =>
      multilines.split('\n').map(
        line => ("  " * level) ++ line
      )
    ).mkString("\n")

  def lines(s: String*) = s.mkString("\n")
  def braced(s: String) = s.b
  def commaIf(i: Int) = if (i > 0) ", " else ""

  def indent2(level: Int, lines: String): String =
    indent(level, lines).drop(level * 2)

  def indent(level: Int, lines: String): String =
    indent(level, lines :: Nil)

  implicit class Codegen1(val r: Seq[Int]) extends AnyVal {
    def each(f: Int => String): String = r map f mkString "\n"
    def semis(f: Int => String): String = r map f mkString "; "
    def commas(f: Int => String): String = r map f mkString ", "
  }

  implicit class Codegen2(val s: String) extends AnyVal {
    def indentBy(by: Int) = codegeneration.indent(by, s)
    def indent = codegeneration.indent(1, s)
    def bracedBy(by: Int) = "{" <> s.indentBy(by + 1) <> "}".indentBy(by) // K&R FTW
    def b = s.bracedBy(0)
    def <>(s2: String) = s + "\n" + s2
    def nl(s2: String) = s <> s2
  }

  def aParams(count: Int) = 1 to count map (i => s"a$i: D, a${i}b: Rt")
  def aArgs(count: Int) = 1 to count map (i => s"a$i,a${i}b")
  def xRevArgs(count: Int) = count to 1 by -1 map (i => s"x$i,x${i}b")

  /** adds 1 to a 0-based index */
  def xArg0(index0: Int) = s"x${index0+1}"
  def xArgB0(index0: Int) = xArg0(index0) + "b"
}
