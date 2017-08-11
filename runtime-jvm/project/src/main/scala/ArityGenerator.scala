package org.unisonweb.codegeneration

object ArityGenerator extends OneFileGenerator("Arity.scala") {
  def source: String =

    "package org.unisonweb.compilation" <>
    "" <>
    "import org.unisonweb.Term.Term" <>
    "" <>
    (0 to N).each { i =>
      s"/** A `Runtime` with just one abstract `apply` function, which takes $i args. */" <>
      s"abstract class Arity$i(decompileIt: => Term) extends Runtime " + {
        "def this(t: TermC, dummy: Unit) = this(unTermC(t))" <>
          "def decompile = decompileIt" <>
          s"def arity: Int = $i" <>
          (0 until i).each { j => s"${applySignature(j)} = ???"} <>
          "// " + applySignature(i) <>
          ((i+1) to N).each { j => s"${applySignature(j)} = " + tailEval(i, "apply") } <>
          "def apply(rec: Rt, xs: Array[Slot], r: R): D = " + tailEvalN(i, "apply")
      }.b <> ""
    } <>
    "abstract class ArityN(val arity: Int, decompileIt: => Term) extends Runtime " + {
      "def this(arity: Int, t: TermC, dummy: Unit) = this(arity, unTermC(t))" <>
      "def decompile = decompileIt" <>
      "" <>
      (0 to N).each { j => s"${applySignature(j)} = ???"} <>
      "// def apply(rec: Rt, xs: Array[Slot], r: R): D"
    }.b
}
