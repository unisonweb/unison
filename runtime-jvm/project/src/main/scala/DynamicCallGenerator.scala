package org.unisonweb.codegeneration

import java.io.File

object DynamicCallGenerator {
  def apply(outDir: File): (File, String) =
    (new File(outDir, "DynamicCall.scala"), source)

  val x = 0 until 10 each { i =>
    s"woot $i"
  } indent 2

 val N = maxInlineArity
 def source =

   "package org.unisonweb" <>
   "" <>
   "import Runtime._" <>
   "import Term.{Term,Name}" <>
   "import annotation.switch" <>
   "" <>
   "object DynamicCall " + {
     "" <>
     "def dynamicCall(fn: Rt, args: Array[Rt], decompile: Term, isTail: Boolean): Rt = " <>
       "if (isTail) dynamicTailCall(fn, args, decompile)".indent <>
       "else dynamicNonTailCall(fn, args, decompile)".indent <>
     "" <>
     "def dynamicTailCall(fn: Rt, args: Array[Rt], decompile: Term): Rt = ???" <>
     "" <>
     "def dynamicNonTailCall(fn: Rt, args: Array[Rt], decompile: Term): Rt = " + {
       "val arity = args.map(_.arity).max" <>
       "val args2 = args" <>
       "(arity: @switch) match " + { (0 to N).each { i =>
          s"case $i => (args.length: @switch) match " + { (1 to N).each { j =>
            s"case $j => " <> {
              (0 until j).each(j => s"val arg$j = args($j)") <>
              { s"new Arity$i(decompile) " + {
                 "def bind(env: Map[Name,Rt]) = ()" <>
                 { "def apply(rec: Rt, " + (1 to i).commas(i => s"x$i: D, x${i}b: Rt") + commaIf(i) + "r: R) = ???" }
                // todo
              }.b }
            }.indent } nl
             "case j => ???"
          }.b } nl
          "case n => ???"
        }.b
     }.b
   }.b
}
