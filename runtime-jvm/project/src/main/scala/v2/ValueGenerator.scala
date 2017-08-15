package org.unisonweb.codegeneration
package v2

object ValueGenerator extends OneFileGenerator("Value.scala") {
  def source =
    "package org.unisonweb.compilation" <>
    "package v2" <>
    "" <>
    "abstract class Value" <>
    "case class Num(d: D) extends Value" <>
    "// abstract class Data extends Value" <>
    "abstract class Lambda extends Value " + {
      (0 to N).each(applySignature) <>
        applyNSignature
    }.b
}
