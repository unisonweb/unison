package org.unisonweb.codegeneration

object LetRecGenerator extends OneFileGenerator("CompileLetRec.scala") {
  def source =
    "package org.unisonweb.compilation" <>
    "" <>
    b("trait CompileLetRec") {
      bEq("def compileLetRec(e: TermC, bindings: Array[Computation], body: Computation): Computation") {
        switch("stackSize(e)") {
           (0 to maxInlineStack).each { stackSize =>
             `case`(stackSize) {
               switch("bindings.length") {
                 (1 to maxInlineArgs).each { argCount =>
                   `case`(argCount) {
                     b(s"class LetRecS${stackSize}A${argCount} extends Computation$stackSize(e, ())") {
                       (1 to argCount).each { k => s"val b$k = bindings(${k-1})" } <>
                       bEq(applySignature(stackSize)) {
                         val bArgs =
                           if (stackSize + argCount <= maxInlineStack)
                             (argCount to 1 by -1).commas(l => s"0.0, b${l}r") + commaIf(stackSize) +
                               (0 until stackSize).commas(l => s"x$l, x${l}b")
                           else
                             "Array(" +
                               (argCount to 1 by -1).commas(l => s"Slot(0.0, b${l}r)") + commaIf(stackSize) +
                               (0 until stackSize).commas(l => s"Slot(x$l, x${l}b)") +
                               ")"
                         "val " + (1 to argCount).commas { k => s"b${k}r" } + " = Ref()" <>
                         (1 to argCount).each { k => s"b${k}r.value = Value(" + catchTC(s"b$k(rec, $bArgs, r)") + ", r.boxed)" } <>
                         s"body(rec, $bArgs, r)"
                       }
                     } <>
                     s"new LetRecS${stackSize}A${argCount}"
                   }
                 } <>
                 `case`("m") {
                   b(s"class LetRecS${stackSize}AM extends Computation$stackSize(e, ())") {
                     bEq(applySignature(stackSize)) {
                       "val refs = Array.fill(m)(Ref())" <>
                         "val xs = Array[Slot](" + (0 until stackSize).commas(k => s"Slot(x$k,x${k}b)") + ")" <>
                         "val slots = refs.view.map(r => Slot(0.0, r)).reverse.toArray ++ xs" <>
                         "var i = 0" <>
                         b("while (i < bindings.length)") {
                           "refs(i).value = Value(" + catchTC("bindings(i)(rec, slots, r)") + ", r.boxed)" <>
                             "i += 1"
                         } <>
                         "body(rec, slots, r)"
                     }
                   } <>
                     s"new LetRecS${stackSize}AM"
                 }.indent
               }
             }
           } <>
           `case`("n") {
             b(s"class LetRecSN extends ComputationN(n, e, ())") {
               bEq(applyNSignature) {
                 "val refs = Array.fill(bindings.length)(Ref())" <>
                 "val slots = refs.view.map(r => Slot(0.0, r)).reverse.toArray ++ xs" <>
                 "var i = 0" <>
                 b("while (i < bindings.length)") {
                   "refs(i).value = Value(" + catchTC("bindings(i)(rec, slots, r)") + ", r.boxed)" <>
                     "i += 1"
                 } <>
                 "body(rec, slots, r)"
               }
             } <>
             "new LetRecSN"
           }
        }
      }
    }
}
