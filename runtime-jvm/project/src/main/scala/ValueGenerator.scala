package org.unisonweb.codegeneration

object ValueGenerator extends OneFileGenerator("Value.scala") {
  def source =
    "package org.unisonweb.compilation" <>
    "" <>
    "abstract class Value { def apply(r: R): D } " <>
    "object Value { def apply(d: D, v: Value): Value = if (v eq null) Num(d) else v }" <>
    "case class Num(d: D) extends Value { def apply(r: R) = d } " <>
    "// abstract class Data extends Value" <>
    "abstract class Lambda extends Value " + {
      "def arity: Int" <>
      "def apply(r: R) = { r.boxed = this; 0.0 }" <>
      (0 to N).each(applySignature) <>
        applyNSignature
    }.b <>
    "case class Ref(var value: Value = null) extends Value " + {
      "def apply(r: R) = value(r)"
    }.b
}
