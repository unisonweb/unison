package org.unisonweb.codegeneration

object LookupVarGenerator extends OneFileGenerator("LookupVar.scala") {
  def source: String =
    "package org.unisonweb.compilation" <>
    "" <>
    "import org.unisonweb.Term.Term" <>
    "" <>
    "trait LookupVar " + {
      "def lookupVar(i: Int, e: Term): Computation = (i: @annotation.switch) match " + {
        (0 until N).each { i =>
          s"case $i => new Computation${i+1}(e) " + {
            s"override ${applySignature(i+1)} = " + {
              s"if (x${i}b eq null) x$i" <>
              s"else x${i}b(r)"
            }.b
          }.b
        } <>
    s"""case i => new ComputationN(i,e) {
       |  override def apply(rec: Lambda, xs: Array[Slot], r: R) = {
       |    val x = xs(i)
       |    if (x.boxed eq null) x.unboxed
       |    else x.boxed(r)
       |  }
       |}
     """.stripMargin
      }.b
    }.b
}
