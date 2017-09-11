package org.unisonweb.codegeneration

object TailCallsGenerator extends OneFileGenerator("TailCalls.scala") {

  def source: String =
    "package org.unisonweb.compilation" <>
    "" <>
    b("trait NoBuildStackTrace extends Throwable") {
      "override def fillInStackTrace = this"
    } <<>>
    s"case class Result(var boxed: Value = null, var fn: Lambda = null, ${(0 until maxInlineTC).commas(i => s"var x$i: D = 0.0, var x${i}b: V = null") + commaIf(maxInlineTC)}var xs: Array[Slot] = null)" <<>>
    "case object TailCall extends NoBuildStackTrace" <>
    "case object SelfCall extends NoBuildStackTrace" <<>>
    b("trait TailCalls") (
      (1 to maxInlineArgs).each { i =>
        bEq("@inline def selfTailCall(" + signatureXArgs(i) + commaIf(i) + "r: R): D")(
          (0 until (i min maxInlineTC)).each(i => s"r.x$i = x$i; r.x${i}b = x${i}b") <>
          (if (i > maxInlineTC)
            "r.xs = Array(" + (maxInlineTC until i).commas(i => s"Slot(x$i, x${i}b)") + ")"
          else "r.xs = null") <>
          "throw SelfCall"
        )
      } <>
      bEq("@inline def selfTailCall(args: Array[Slot], r: R): D") {
        (0 until maxInlineTC).each(i => s"r.x$i = args($i).unboxed; r.x${i}b = args($i).boxed") <>
        includeLineIf(maxInlineTC > 0)(s"r.xs = args.drop($maxInlineTC)") +
        "throw SelfCall"
      } <<>>
      (1 to maxInlineArgs).each { i =>
        bEq("@inline def tailCall(fn: Lambda, " + signatureXArgs(i) + commaIf(i) + "r: R): D")(
          "r.fn = fn" <>
          (0 until (i min maxInlineTC)).each(i => s"r.x$i = x$i; r.x${i}b = x${i}b") <>
          (if (i > maxInlineTC)
            "r.xs = Array(" + (maxInlineTC until i).commas(i => s"Slot(x$i, x${i}b)") + ")"
          else "r.xs = null") <>
          "throw TailCall"
        )
      } <>
      bEq("@inline def tailCall(fn: Lambda, args: Array[Slot], r: R): D") (
        "r.fn = fn" <>
        (0 until maxInlineTC).each(i => s"r.x$i = args($i).unboxed; r.x${i}b = args($i).boxed") <>
        includeLineIf(maxInlineTC > 0)(s"r.xs = args.drop($maxInlineTC)") +
        "throw TailCall"
      ) <<>>

      bEq("def loop(r: R): D") {
        b("while (true)") {
          "val fn = r.fn" <>
          b("try") {
            "return " + switch("fn.arity") {
              (1 to maxInlineArgs).each {
                case i if i <= maxInlineTC => s"case $i => fn(fn, " +
                  (0 until i).commas(i => s"r.x$i, r.x${i}b") + ", r)"
                case i => s"case $i => fn(fn, " +
                  (0 until maxInlineTC).commas(i => s"r.x$i, r.x${i}b") + ", " +
                  (0 until (i - maxInlineTC)).commas(i => s"r.xs($i).unboxed, r.xs($i).boxed") + ", r)"
              } <>
              (if (maxInlineArgs > 0)
                "case n => fn(fn, Array(" +
                  (0 until maxInlineTC).commas(i => s"Slot(r.x$i, r.x${i}b)") + ") ++ r.xs, r)"
              else
                "case n => fn(fn, r.xs, r)"
                )
            }
          } <>
          "catch { case tc2: TC => }"
        } <>
        "0.0"
      }
    )
}
