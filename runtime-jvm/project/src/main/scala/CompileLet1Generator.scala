package org.unisonweb.codegeneration

object CompileLet1Generator extends OneFileGenerator("CompileLet1.scala") {
  def source: String =
    "package org.unisonweb.compilation" <>
    "" <>
    b("trait CompileLet1") {
      bEq("def compileLet1(e: TermC, binding: Computation, body: Computation)") {
        "val stackSize = binding.stackSize max body.stackSize" <>
        switch("stackSize") {
          (0 until maxInlineStack).each { stackSize =>
            val className = s"Let1S${stackSize}"
            `case`(stackSize) {
              b(s"class $className extends Computation${stackSize}(e,())") {
                bEq(applySignature(stackSize)) {
                  "val b = " + eval(stackSize, "binding") <>
                  "val br = r.boxed" <>
                  "body(rec, b, br, " + xArgs(stackSize) + commaIf(stackSize) + "r)"
                }
              } <>
              s"new $className"
            }
          } <>
          `case`(maxInlineStack) {
            val className = s"Let1S${maxInlineStack}"
            b(s"class $className extends Computation${maxInlineStack}(e,())") {
              bEq(applySignature(maxInlineStack)) {
                "val b = " + eval(maxInlineStack, "binding") <>
                "val br = r.boxed" <>
                ("body(rec, Array(Slot(b, br)"
                  + commaIf(maxInlineStack)
                  + (0 until maxInlineStack).commas(slot)
                  + "), r)")
              }
            } <>
            s"new $className"
          } <>
          `case`("stackSize") {
            val className = s"Let1SN"
            b(s"class $className extends ComputationN(stackSize, e, ())") {
              bEq(applyNSignature) {
                "// evaluate binding and push onto stack for evaluating body" <>
                "val b = " + evalN("binding") <>
                "val br = r.boxed" <>
                "body(rec, Slot(b, br) +: xs, r)"
              }
            } <>
            s"new $className"
          }
        }
      }
    }
}