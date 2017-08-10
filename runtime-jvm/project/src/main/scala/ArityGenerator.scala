package org.unisonweb.codegeneration

import java.io.File

object ArityGenerator {
  def apply(outDir: File): (File, String) =
    (new File(outDir, "Arities.scala"), source)

  val N = maxInlineArity

  def source =

    "package org.unisonweb.compilation" <>
      "" <>
      "import org.unisonweb.Term.Term" <>
      "" <>
      (0 to N).each { i =>
        s"/** A `Runtime` with just one abstract `apply` function, which takes $i args. */" <>
        s"abstract class Arity$i(decompileIt: => Term) extends Runtime" + {
          "def this(t: TermC, dummy: Unit) = this(unTermC(t))" <>
            "def decompile = decompileIt" <>
            s"def arity: Int = $i" <>
            (0 until i).each { j => s"${applySignature(j)} = ???"} <>
            "// " + applySignature(i) <>
            ((i+1) to N).each { j => s"${applySignature(j)} = " + tailEval(i, "apply") } <>
            "def apply(rec: Rt, xs: Array[Slot], r: R): D =" <>
              tailEvalN(i, "apply").indent
        }.b <>
        ""
      } <>
      "" <>
      """abstract class ArityN(val arity: Int, decompileIt: => Term) extends Runtime { self =>
        |  def this(arity: Int, t: TermC, dummy: Unit) = this(arity, unTermC(t))
        |  def decompile = decompileIt
        |
        |  def apply(rec: Rt, r: R): D = ???
        |  def apply(rec: Rt, x0: D, x0b: Rt, r: R): D = ???
        |  def apply(rec: Rt, x0: D, x0b: Rt, x1: D, x1b: Rt, r: R): D = ???
        |  def apply(rec: Rt, x0: D, x0b: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R): D = ???
        |  def apply(rec: Rt, x0: D, x0b: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R): D = ???
        |}""".stripMargin

}
