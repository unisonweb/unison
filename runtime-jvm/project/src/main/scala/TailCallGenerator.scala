package org.unisonweb.codegeneration

import java.io.File

object TailCallGenerator {
  val N = maxInlineArity
  def apply(outDir: File): (File, String) = (new File(outDir, "TailCalls.scala"), source)

  def slot(i: Int) = s"Slot(x$i,x${i}b)"

  def source: String =

    "package org.unisonweb.compilation" <>
    "" <>
    "trait TailCalls " + {
      (1 to N).each { i =>
        "@inline def selfTailCall(" + (0 until i).commas(i => s"x$i: D, x${i}b: Rt") + commaIf(i) + "r: R): D =" <>
          (i match {
            case 1 => s"throw new SelfCall(${xArgs(1)}, 0.0, null, null)"
            case 2 => s"throw new SelfCall(${xArgs(2)}, null)"
            case j => s"throw new SelfCall(${xArgs(2)}, Array(" + (2 until j).commas(slot) + "))"
          }).indent
      } <>
    """@inline def selfTailCall(args: Array[Slot], r: R): D =
      |  throw new SelfCall(args(0).unboxed, args(0).boxed, args(1).unboxed, args(1).boxed, args.drop(2))
      |""".stripMargin <>
      //
      (1 to N).each { i =>
        "@inline def tailCall(fn: Rt, " + (0 until i).commas(i => s"x$i: D, x${i}b: Rt") + commaIf(i) + "r: R): D =" <>
          (i match {
            case 1 => s"throw new TailCall(fn, ${xArgs(1)}, 0.0, null, null)"
            case 2 => s"throw new TailCall(fn, ${xArgs(2)}, null)"
            case j => s"throw new TailCall(fn, ${xArgs(2)}, Array(" + (2 until j).commas(slot) + "))"
          }).indent
      } <>
    """@inline def tailCall(fn: Rt, args: Array[Slot], r: R): D =
      |  throw new TailCall(fn, args(0).unboxed, args(0).boxed, args(1).unboxed, args(1).boxed, args.drop(2))
      |""".stripMargin <>
      //
      s"""def loop(tc0: TailCall, r: R): D = {
        |  var tc = tc0
        |  while (!(tc eq null)) {
        |    val fn = tc.fn
        |    try {
        |      return ((fn.arity : @annotation.switch) match {
        |${(1 to N).each {
        case i if i <= 2 => s"case $i => fn(fn, " + (1 to i).commas(i => s"tc.x$i, tc.x${i}b") + ", r)"
        case i => s"case $i => fn(fn, " +
          (1 to 2).commas(i => s"tc.x$i, tc.x${i}b") + ", " +
          (0 to (i-3)).commas(i => s"tc.args($i).unboxed, tc.args($i).boxed") + ", r)"
          }.indentBy(4)}
        |        case n => fn(fn, Array(Slot(tc.x1, tc.x1b), Slot(tc.x2, tc.x2b)) ++ tc.args, r)
        |      })
        |    }
        |    catch { case tc2: TailCall => tc = tc2 }
        |  }
        |  0.0
        |}
        |""".stripMargin
    }.b



}