package org.unisonweb.codegeneration

object CompileIf0Generator extends OneFileGenerator("CompileIf0.scala") {
  def source: String =
    "package org.unisonweb.compilation" <>
      "" <>
      "import org.unisonweb.Term.Term" <>
      "" <>
      b("trait CompileIf0") {
        bEq("def compileIf0(cond: Computation, if0: Computation, ifNot0: Computation, term: Term)") {
          "val stackSize = cond.stackSize max if0.stackSize max ifNot0.stackSize" <>
          switch("stackSize") {
            (0 to maxInlineStack).each { stackSize =>
              `case`(s"/* stackSize = */ $stackSize") {
                b(s"class If0S$stackSize extends Computation$stackSize(term)") {
                  bEqExpr(applySignature(stackSize)) {
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
              b("class If0SN extends ComputationN(stackSize, term)") {
                bEqExpr(applyNSignature) {
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