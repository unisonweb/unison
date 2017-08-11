package org.unisonweb.codegeneration

import java.io.File

object LookupVarGenerator {
  def apply(outDir: File): (File, String) =
    (new File(outDir, "LookupVar.scala"), source)

  val N = maxInlineArity

  def source: String =
    "package org.unisonweb.compilation" <>
    "" <>
    "import org.unisonweb.Term.{Name, Term}" <>
    "" <>
    "trait LookupVar " + {
      "def lookupVar(i: Int, name: Name, e: Term): Rt = i match " + {
        (0 until N).each { i =>
          s"case $i => new Arity${i+1}(e) " + {
             "override def bind(env: Map[Name,Rt]) = ()" <>
            s"override ${applySignature(i+1)} = " + {
              s"if (!(x${i}b eq null)) r.boxed = x${i}b" <>
              s"x$i"
            }.b
          }.b
        } <>
    s"""case i => new ArityN(i,e) {
       |  override def bind(env: Map[Name,Rt]) = ()
       |  override def apply(rec: Rt, xs: Array[Slot], result: R) = {
       |    val x = xs(i)
       |    if (!(x.boxed eq null)) result.boxed = x.boxed
       |    x.unboxed
       |  }
       |}
     """.stripMargin
      }.b
    }.b
}

