package org.unisonweb.codegeneration

object ComputationGenerator extends OneFileGenerator("Computation.scala") {
  def source =
    "package org.unisonweb.compilation" <>
    "" <>
    "import org.unisonweb.Term.Term" <>
    "" <>
    "abstract class Computation " + {
      "def stackSize: Int" <>
      (0 to N).each(applySignature) <>
      applyNSignature
    }.b <>
    (0 to N).each { i =>
      s"/** A `Computation` with just one abstract `apply` function, which takes $i args. */" <>
      s"abstract class Computation$i(decompileIt: => Term) extends Computation " + {
        "def this(t: TermC, dummy: Unit) = this(unTermC(t))" <>
          "def decompile = decompileIt" <>
          s"def stackSize: Int = $i" <>
          (0 until i).each { j => s"""${applySignature(j)} = throw new Exception("Expected $i args via stack, but given $j.")""" } <>
          "// " + applySignature(i) <>
          ((i+1) to N).each { j => s"${applySignature(j)} = " + tailEval(i, "apply") } <>
          applyNSignature + " = " + tailEvalN(i, "apply")
      }.b <> ""
    } <>
    "abstract class ComputationN(val stackSize: Int, decompileIt: => Term) extends Computation " + {
      "def this(stackSize: Int, t: TermC, dummy: Unit) = this(stackSize, unTermC(t))" <>
      "def decompile = decompileIt" <>
      "" <>
      (0 to N).each { j => s"""${applySignature(j)} = throw new Exception("Expected ${N+1}+ args via stack, but given $j.")""" } <>
      "// " + applyNSignature
    }.b
}
