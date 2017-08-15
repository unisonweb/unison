package org.unisonweb.codegeneration
package v2

object ComputationGenerator extends OneFileGenerator("Computation.scala") {
  def source =
    "package org.unisonweb.compilation" <>
    "package v2" <>
    "" <>
    "import org.unisonweb.Term.Name" <>
    "" <>
    "abstract class Computation " + {
      "def stackSize: Int" <>
      "def bind(env: Map[Name, Value]): Unit" <>
      (0 to N).each { i =>
        applySignature(i)
      } <>
      applyNSignature
    }.b

}
