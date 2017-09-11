package org.unisonweb.codegeneration

object TailCallsGenerator extends OneFileGenerator("TailCalls.scala") {

  def source: String =
    "package org.unisonweb.compilation" <>
    "" <>
    b("class TailCall(val fn: Lambda, "
      + (0 until maxInlineTC).commas(i => s"val x$i: D, val x${i}b: Value")
      + commaIf(maxInlineTC) + "val args: Array[Slot]) extends Throwable") {
      "override def fillInStackTrace = this"
    } <<>>
    b("case class SelfCall("
      + (0 until maxInlineTC).commas(i => s"x$i: D, x${i}b: Value")
      + commaIf(maxInlineTC)
      + "args: Array[Slot]) extends Throwable") {
      "override def fillInStackTrace = this"
    } <<>>
    b("trait TailCalls") {
      (1 to maxInlineArgs).each { i =>
        "@inline def selfTailCall(" + (0 until i).commas(i => s"x$i: D, x${i}b: V") + commaIf(i) + "r: R): D =" <>
          (if (i < maxInlineTC)
            s"throw new SelfCall(" + List(xArgs(i), (i+1 to maxInlineTC).commas(_ => "0.0, null")).mkString(", ") + ", Array())"
          else
            s"throw new SelfCall(" + xArgs(maxInlineTC) + ", Array(" + (maxInlineTC until i).commas(slot) + "))"
          ).indent
      } <>
      "@inline def selfTailCall(args: Array[Slot], r: R): D =" <>
        ("throw new SelfCall(" +
          (0 until maxInlineTC).commas(i => s"args($i).unboxed, args($i).boxed")
          + commaIf(maxInlineTC)
          + (if (maxInlineTC > 0) s"args.drop($maxInlineTC)" else "args")
          + ")"
          ).indent <>
      "" <>
      (1 to maxInlineStack).each { i =>
        "@inline def tailCall(fn: Lambda, " + (0 until i).commas(i => s"x$i: D, x${i}b: V") + commaIf(i) + "r: R): D =" <>
          (if (i < maxInlineTC)
            s"throw new TailCall(fn, " + List(xArgs(i), (i+1 to maxInlineTC).commas(_ => "0.0, null")).mkString(", ") + ", Array())"
          else
            s"throw new TailCall(fn, " + xArgs(maxInlineTC) + ", Array(" + (maxInlineTC until i).commas(slot) + "))"
          ).indent
      } <>
      "@inline def tailCall(fn: Lambda, args: Array[Slot], r: R): D =" <>
        ("throw new TailCall(fn, " +
          (0 until maxInlineTC).commas(i => s"args($i).unboxed, args($i).boxed")
          + commaIf(maxInlineTC)
          + (if (maxInlineTC > 0) s"args.drop($maxInlineTC)" else "args")
          + ")"
          ).indent <>
      "" <>
      bEq("def loop(tc0: TailCall, r: R): D") {
        "var tc = tc0" <>
        b("while (!(tc eq null))") {
          "val fn = tc.fn" <>
          b("try") {
            "return " + switch("fn.arity") {
              (1 to maxInlineArgs).each {
                case i if i <= 2 => s"case $i => fn(fn, " + (0 until i).commas(i => s"tc.x$i, tc.x${i}b") + ", r)"
                case i => s"case $i => fn(fn, " +
                  (0 until maxInlineTC).commas(i => s"tc.x$i, tc.x${i}b") + ", " +
                  (0 to (i - maxInlineTC - 1)).commas(i => s"tc.args($i).unboxed, tc.args($i).boxed") + ", r)"
              } <>
              (if (maxInlineArgs > 0)
                "case n => fn(fn, Array(" +
                  (0 until maxInlineTC).commas(i => s"Slot(tc.x$i, tc.x${i}b)") + ") ++ tc.args, r)"
                else
                "case n => fn(fn, tc.args, r)"
              )
            }
          } <>
          "catch { case tc2: TailCall => tc = tc2 }"
        } <>
        "0.0"
      }
    }
}
