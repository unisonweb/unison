package org.unisonweb.codegeneration

object ComputationGenerator extends OneFileGenerator("Computation.scala") {
  def source =
    "package org.unisonweb.compilation" <>
    "" <>
    "import org.unisonweb.Term.Term" <>
    "" <>
    b("abstract class Computation") {
      "def decompile: Term" <>
      "def stackSize: Int" <>
      (0 to maxInlineStack).each(applySignature) <>
      applyNSignature
    } <<>>
    b("case class Return(value: Value)(decompileIt: => Term) extends Computation0(decompileIt)") {
      s"${applySignature(0)} = value(r)"
    } <<>>
    b("case class LazyReturn(value: () => Value)(decompileIt: => Term) extends Computation0(decompileIt)") {
      s"${applySignature(0)} = value()(r)"
    } <<>>
    (0 to maxInlineStack).eachNL { i =>
      s"/** A `Computation` with just one abstract `apply` function, which takes $i args. */" <>
      b(s"abstract class Computation$i(decompileIt: => Term) extends Computation") {
        "def this(t: TermC, dummy: Unit) = this(unTermC(t))" <>
          "def decompile = decompileIt" <>
          s"def stackSize: Int = $i" <>
          (0 until i).each { j => s"""${applySignature(j)} = throw new Exception("Expected $i stack elements, but given $j.")""" } <>
          "// " + applySignature(i) <>
          ((i+1) to maxInlineStack).each { j => s"${applySignature(j)} = " + tailEval(i, "apply") } <>
          applyNSignature + " = " + tailEvalN(i, "apply")
      }
    } <<>>
    b("abstract class ComputationN(val stackSize: Int, decompileIt: => Term) extends Computation") {
      "def this(stackSize: Int, t: TermC, dummy: Unit) = this(stackSize, unTermC(t))" <>
      "def decompile = decompileIt" <>
      "" <>
      (0 to maxInlineStack).each { j => s"""${applySignature(j)} = throw new Exception("Expected ${maxInlineStack+1}+ stack elements, but given $j.")""" } <>
      "// " + applyNSignature
    }
}
