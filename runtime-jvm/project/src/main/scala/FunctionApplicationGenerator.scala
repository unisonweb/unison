package org.unisonweb.codegeneration

object FunctionApplicationGenerator extends OneFileGenerator("FunctionApplication.scala") {
  def source =
    "package org.unisonweb.compilation" <>
    "" <>
    "import org.unisonweb.Term.Term" <>
    "" <>
    b("trait FunctionApplication") {
      bEqExpr("def staticCall(fn: Lambda, args: Array[Computation], decompile: Term, isTail: Boolean): Computation") {
        "if (isTail) staticTailCall(fn, args, decompile)" <>
        "else staticNonTail(fn, args, decompile)"
      } <>
      "" <>
      bEqExpr("def staticRecCall(args: Array[Computation], decompile: Term, isTail: Boolean): Computation") {
        "if (isTail) staticRecTailCall(args, decompile)" <>
        "else staticRecNonTail(args, decompile)"
      } <>
      "" <>
      make("staticNonTail",
        classPrefix = "StaticNonTail",
        declArgsPrefix = Some("fn: Lambda"),
        evalFn = "fn",
        evalArgsPrefix = Some("fn")
      ) <>
      "" <>
      make("staticTailCall",
        classPrefix = "StaticTailCall",
        declArgsPrefix = Some("fn: Lambda"),
        evalFn = "tailCall",
        evalArgsPrefix = Some("fn")
      ) <>
      "" <>
      make("staticRecNonTail",
        classPrefix = "StaticRecNonTail",
        declArgsPrefix = None,
        evalFn = "rec",
        evalArgsPrefix = Some("rec")
      ) <>
      "" <>
      make("staticRecTailCall",
        classPrefix = "StaticRecTailCall",
        declArgsPrefix = None,
        evalFn = "selfTailCall",
        evalArgsPrefix = None
      )
    }

  def make(defName: String, classPrefix: String, declArgsPrefix: Option[String], evalFn: String, evalArgsPrefix: Option[String]) = {
    val evalArgsPrefixStr = evalArgsPrefix.map(_ + ", ").getOrElse("")
    def eEvalArgs(argCount: Int) =
      evalArgsPrefixStr + (argCount - 1 to 0 by -1).commas(i => s"e${i}, e${i}b") + commaIf(argCount) + "r"

    def xEvalArgs(argCount: Int) =
      "rec, " + (0 until argCount).commas(i => s"x${i}, x${i}b") + commaIf(argCount) + "r"

    bEq(s"def $defName(" + declArgsPrefix.map(_ + ", ").getOrElse("") + "args: Array[Computation], decompile: Term): Computation") {
      "val stackSize = args.map(_.stackSize).max" <>
      switch("stackSize") {
          (0 to maxInlineStack).each { stackSize =>
            `case`(stackSize) {
              switch("args.length") {
                (1 to maxInlineArgs).each { argCount =>
                  `case`(argCount) {
                    (0 until argCount).each { i => s"val arg$i = args($i)" } <>
                    b(s"class ${classPrefix}S${stackSize}A${argCount} extends Computation${stackSize}(decompile)") {
                      bEq(applySignature(stackSize)) {
                        (0 until argCount).each(i => (
                          s"val e$i = "
                            + catchTC(s"arg$i(${xEvalArgs(stackSize)})")
                            + s"; val e${i}b = r.boxed")) <>
                          s"$evalFn(${eEvalArgs(argCount)})"
                      }
                    } <>
                    s"new ${classPrefix}S${stackSize}A${argCount}" <>
                    ""
                  }
                } <>
                `case`("argCount") {
                  b(s"class ${classPrefix}S${stackSize}AN extends Computation${stackSize}(decompile)") {
                    bEq(applySignature(stackSize)) {
                      "val slots = new Array[Slot](argCount)" <>
                      "var i = 0" <>
                      b("while (i < argCount)") {
                        "val slot = slots(argCount - 1 - i)" <>
                        s"slot.unboxed = " + catchTC(s"args(i)(${xEvalArgs(stackSize)})") <>
                        s"slot.boxed = r.boxed" <>
                        "i += 1"
                      } <>
                      s"$evalFn(${evalArgsPrefixStr}slots, r)"
                    }
                  } <>
                  s"new ${classPrefix}S${stackSize}AN"
                }
              }
            }
          } <>
          `case`("stackSize") {
            switch("args.length") {
              (1 to maxInlineArgs).each { argCount =>
                `case`(argCount) {
                  (0 until argCount).each { i => s"val arg$i = args($i)" } <>
                    b(s"class ${classPrefix}SNA$argCount extends ComputationN(stackSize, decompile)") {
                      bEq(applyNSignature) {
                        (0 until argCount).each { i =>
                          s"val e$i = " + catchTC(s"arg$i(rec, xs, r)") + s"; val e${i}b = r.boxed"
                        } <>
                        s"$evalFn(${eEvalArgs(argCount)})"
                      }
                    } <>
                    s"new ${classPrefix}SNA$argCount"
                }
              } <>
              `case`("argCount") {
                b(s"class ${classPrefix}SNAN extends ComputationN(stackSize, decompile)") {
                  bEq(applyNSignature) {
                    "val slots = new Array[Slot](argCount)" <>
                      "var i = 0" <>
                      b("while (i < argCount)") {
                        "val slot = slots(argCount - 1 - i)" <>
                          s"slot.unboxed = " + catchTC(s"args(i)(rec, xs, r)") <>
                          s"slot.boxed = r.boxed" <>
                          "i += 1"
                      } <>
                      s"$evalFn(${evalArgsPrefixStr}slots, r)"
                  }
                } <>
                s"new ${classPrefix}SNAN"
              }
            }
          }
        }
    }
  }
}
