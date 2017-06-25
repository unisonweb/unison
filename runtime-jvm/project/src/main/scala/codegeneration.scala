package org.unisonweb

package object codegeneration {

  val maxInlineArity = 4

  def indent(level: Int, lines: Seq[String]): String =
    lines.flatMap(multilines =>
      multilines.split('\n').map(
        line => ("  " * level) ++ line
      )
    ).mkString("\n")

  def indent(level: Int, lines: String): String =
    indent(level, lines :: Nil)

  def aParams(count: Int) = 1 to count map (i => s"a$i: D, a${i}b: Rt")
  def aArgs(count: Int) = 1 to count map (i => s"a$i,a${i}b")
  def xRevArgs(count: Int) = count to 1 by -1 map (i => s"x$i,x${i}b")

  /** adds 1 to a 0-based index */
  def xArg0(index0: Int) = s"x${index0+1}"
  def xArgB0(index0: Int) = xArg0(index0) + "b"
}
