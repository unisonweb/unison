package org.unisonweb.codegeneration

object CompileIf0Generator extends OneFileGenerator("CompileIf0.scala") {
  def source: String =
    "package org.unisonweb.compilation" <>
      "" <>
      b("trait CompileIf0") {
        bEq("def compileIf0(e: TermC, cond: Computation, if0: Computation, ifNot0: Computation)") {
          switch("stackSize(e)") {
            (0 to maxInlineStack).each { stackSize =>
              `case`(s"/* stackSize = */ $stackSize") {
                b(s"class If0S$stackSize extends Computation$stackSize(e,())") {
                  indentEqExpr(applySignature(stackSize)) {
                    "if ((" + eval(stackSize, "cond") + ") == 0.0)" <>
                      tailEval(stackSize, "if0").indent <>
                    "else " <>
                      tailEval(stackSize, "ifNot0").indent
                  }
                } <>
                s"new If0S$stackSize"
              }
            } <>
            `case`("stackSize") {
              b("class If0SN extends ComputationN(stackSize,e,())") {
                indentEqExpr(applyNSignature) {
                  "if ((" + evalN("cond") + ") == 0.0)" <>
                    tailEvalN("if0").indent <>
                    "else " <>
                    tailEvalN("ifNot0").indent
                }
              } <>
                s"new If0SN"
            }
          }
        }
      }
}