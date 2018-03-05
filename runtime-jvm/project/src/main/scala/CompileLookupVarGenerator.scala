package org.unisonweb.codegeneration

object CompileLookupVarGenerator extends OneFileGenerator("CompileLookupVar.scala") {
  def source: String = (
    "package org.unisonweb.compilation" <>
    "" <>
    "import org.unisonweb.Term.Term" <>
    "" <>
    b("trait CompileLookupVar") {
      bEq("def compileLookupVar(i: Int, decompile: Term): Computation") {
        switch("i") {
          (0 until maxInlineStack).eachNL { i =>
            `case`(i) {
              b(s"class LookupVar$i extends Computation${i+1}(decompile)") {
                s"${applySignature(i+1)} =" <>
                  s"if (x${i}b eq null) { r.boxed = null; x$i } else x${i}b(r)".indent
              } <>
              s"new LookupVar$i"
            }
          } <<>>
          `case`("i") {
            b("class LookupVarN extends ComputationN(i+1, decompile)") {
              bEq(applyNSignature) {
                "val x = xs(i)" <>
                "if (x.boxed eq null) { r.boxed = null; x.unboxed }" <>
                "else x.boxed(r)"
              }
            } <>
            "new LookupVarN"
          }
        }
      } <<>>
      "// This version of variable lookup does not dereference boxed values " <>
      bEq("def compileLookupRef(i: Int): ParamLookup") {
        switch("i") {
          (0 until maxInlineStack).eachNL { i =>
            `case`(i) {
              b(s"class LookupRef$i extends ParamLookup${i+1}") {
                s"${applySignatureP(i+1)} = x${i}b"
              } <>
              s"new LookupRef$i"
            }
          } <<>>
          `case`("i") {
            b("class LookupRefN extends ParamLookupN(i+1)") {
              applyNSignatureP + " = xs(i).boxed"
            } <>
            "new LookupRefN"
          }
        }
      }
    }
  )
}
