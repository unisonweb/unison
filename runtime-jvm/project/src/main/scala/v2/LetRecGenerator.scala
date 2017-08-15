package org.unisonweb.codegeneration
package v2

object LetRecGenerator extends OneFileGenerator("CompileLetRec.scala") {
  def source =
    "package org.unisonweb.compilation" <>
    "package v2" <>
    "" <>
    "trait CompileLetRec " + {
      "def compileLetRec(e: TermC, bindings: Array[Computation], body: Computation): Computation = " + {
        "(stackSize(e): @annotation.switch) match " + {
           (0 to N).each { i =>
             s"case $i => (bindings.length: @annotation.switch) match " + {
               (1 to N).each { j =>
                 s"case $j => " <> {
                   s"class LetRecS${i}A${j} extends Computation$i(e, ()) " + {
                     (1 to j).each { k => s"val b$k = bindings(${k-1})" } <>
                     applySignature(i) + " = " + {
                       val bArgs = (j to 1 by -1).commas(l => s"0.0, b${l}r")
                       (1 to j).each { k => s"val b${k}r = Ref()" } <>
                       // totally wrong - need to push bArgs onto existing stack, and deal with overflow
                       (1 to j).each { k => s"b${k}r.value = Value(b$k(rec, $bArgs, r), r.boxed)" } <>
                       s"body(rec, $bArgs, r)"
                     }.b
                   }.b <>
                   s"new LetRecS${i}A${j}"
                 }.indent
               } <>
               "case n => " <> {
                 s"class LetRecS${i}AN extends ComputationN($i, e, ()) " + {
                   applyNSignature + " = " + {
                     "???"
                   }.b
                 }.b <>
                 s"new LetRecS${i}AN"
               }.indent
             }.b
           }
        }.b
      }.b
    }.b

  //def letrec2(e: TermC, b1: Computation, b2: Computation, body: Computation): Computation =
  //  stackSize(e) match {
  //    case 0 => new Computation0(unTermC(e)) {
  //      override def apply(rec: Lambda, r: R): D = {
  //        val b1r = Ref()
  //        val b2r = Ref()
  //        b1r.value = Value(b1(rec, 0.0, b2r, 0.0, b1r, r), r.boxed)
  //        b2r.value = Value(b2(rec, 0.0, b2r, 0.0, b1r, r), r.boxed)
  //        body(rec, 0.0, b2r, 0.0, b1r, r)
  //      }
  //    }
  //  }

}
