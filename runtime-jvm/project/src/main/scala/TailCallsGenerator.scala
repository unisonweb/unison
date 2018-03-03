package org.unisonweb.codegeneration

object TailCallsGenerator extends OneFileGenerator("TailCalls.scala") {

  def source: String =
    "package org.unisonweb.compilation" <>
    "" <>
    s"case class Result(var boxed: Value = null, var fn: Lambda = null, ${(0 until maxInlineTC).commas(i => s"var x$i: D = 0.0, var x${i}b: V = null") + commaIf(maxInlineTC)}var xs: Array[Slot] = null)" <<>>
    "case object TailCall extends Throwable { override def fillInStackTrace = this }" <>
    "case object SelfTailCall extends Throwable { override def fillInStackTrace = this }" <<>>
    b("trait TailCalls") (
      "// todo: decide whether to clear all unused fields or none; speed vs GCbility" <>
      (1 to maxInlineArgs).each { i =>
        bEq("@inline def selfTailCall(" + signatureXArgs(i) + commaIf(i) + "r: R): D")(
          (0 until (i min maxInlineTC)).each(i => s"r.x$i = x$i; r.x${i}b = x${i}b.toValue(r)") <>
          (if (i > maxInlineTC)
            "// here, r.xs holds only the excess args" <>
            "r.xs = Array(" + (maxInlineTC until i).commas(i => s"Slot(x$i, x${i}b)") + ")"
          else "r.xs = null") <>
          "throw SelfTailCall"
        )
      } <<>>
      bEq("@inline def selfTailCall(args: Array[Slot], r: R): D") {
        "// here, r.xs holds all of the args, to avoid extra unboxing/boxing" <>
        "r.xs = args" <>
        "throw SelfTailCall"
      } <<>>
      (1 to maxInlineArgs).each { i =>
        bEq("@inline def tailCall(fn: Lambda, " + signatureXArgs(i) + commaIf(i) + "r: R): D")(
          "r.fn = fn" <>
          (0 until (i min maxInlineTC)).each(i => s"r.x$i = x$i; r.x${i}b = x${i}b.toValue(r)") <>
          (if (i > maxInlineTC)
            "// here, r.xs holds only the excess args" <>
            "r.xs = Array(" + (maxInlineTC until i).commas(i => s"Slot(x$i, x${i}b)") + ")"
          else "r.xs = null") <>
          "throw TailCall"
        )
      } <>
      bEq("@inline def tailCall(fn: Lambda, args: Array[Slot], r: R): D") (
        "r.fn = fn" <>
        "// here, r.xs holds all of the args, to avoid extra unboxing/boxing" <>
        "r.xs = args" <>
        "throw TailCall"
      ) <<>>

      bEq("def loop(r: R): D") {
        b("while (true)") {
          "val fn = r.fn" <>
          b("try") {
            "return " + switch("fn.arity") {
              (1 to maxInlineArgs).each {
                case i if i <= maxInlineTC => s"case $i => fn(fn" +
                  commaIf(i) +
                  (0 until i).commas(i => s"r.x$i, r.x${i}b") + ", r)"
                case i => s"case $i => fn(fn" +
                  commaIf(maxInlineTC) +
                  (0 until maxInlineTC).commas(i => s"r.x$i, r.x${i}b") + ", " +
                  (0 until (i - maxInlineTC)).commas(i => s"r.xs($i).unboxed, r.xs($i).boxed") + ", r)"
              } <>
              "case n => fn(fn, r.xs, r)"
            }
          } <>
          "catch { case TailCall => }"
        } <>
        "0.0"
      }
    )
}
