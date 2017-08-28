package org.unisonweb.codegeneration

object LookupVarGenerator extends OneFileGenerator("LookupVar.scala") {
  def source: String =
    "package org.unisonweb.compilation" <>
    "" <>
    "import org.unisonweb.Term.Term" <>
    "" <>
    b("trait LookupVar") {
      bEq("def lookupVar(i: Int, e: Term): Computation") {
        switch("i") {
          (0 until maxInlineStack).each { i =>
            `case`(i) {
              b(s"class LookupVar$i extends Computation${i+1}(e)") {
                s"${applySignature(i+1)} =" <>
                  s"if (x${i}b eq null) x$i else x${i}b(r)".indent
              } <>
              s"new LookupVar$i"
            }.<>|
          } <>
          `case`("i") {
            b("class LookupVarN extends ComputationN(i+1, e)") {
              bEq(applyNSignature) {
                "val x = xs(i)" <>
                "if (x.boxed eq null) x.unboxed" <>
                "else x.boxed(r)"
              }
            } <>
            "new LookupVarN"
          }
        }
      }
    }
}
