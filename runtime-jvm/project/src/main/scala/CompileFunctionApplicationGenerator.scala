package org.unisonweb.codegeneration

object CompileFunctionApplicationGenerator extends OneFileGenerator("CompileFunctionApplication.scala") {
  sealed trait EvalFnType
  object EvalFnType {
    case object Lambda extends EvalFnType
    case object TailCall extends EvalFnType
    case object SelfTailCall extends EvalFnType
  }
  def source =
    "package org.unisonweb.compilation" <>
    "" <>
    includeIf(traceEval)(
    "import org.unisonweb.Render1" <>
    "".<>|) +
    b("trait CompileFunctionApplication") {
      indentEqExpr("def staticCall(e: TermC, fn: Lambda, args: Array[Computation], isTail: IsTail): Computation") {
        "if (isTail) staticTailCall(e, fn, args)" <>
        "else staticNonTail(e, fn, args)"
      } <<>>
      indentEqExpr("def staticRecCall(e: TermC, args: Array[Computation], isTail: IsTail): Computation") {
        "if (isTail) staticRecTailCall(e, args)" <>
        "else staticRecNonTail(e, args)"
      } <<>>
      indentEqExpr("def dynamicCall(e: TermC, mkFn: Computation, args: Array[Computation], isTail: IsTail): Computation") {
        "if (isTail) dynamicTailCall(e, mkFn, args)" <>
        "else dynamicNonTailCall(e, mkFn, args)"
      } <<>>
      sourceStatic("staticNonTail",
        classPrefix = "StaticNonTail",
        declArgsPrefix = Some("fn: Lambda"),
        evalFn = "fn",
        evalFnType = EvalFnType.Lambda,
        evalArgsPrefix = Some("fn")
      ) <<>>
      sourceStatic("staticTailCall",
        classPrefix = "StaticTailCall",
        declArgsPrefix = Some("fn: Lambda"),
        evalFn = "tailCall",
        evalFnType = EvalFnType.TailCall,
        evalArgsPrefix = Some("fn")
      ) <<>>
      sourceStatic("staticRecNonTail",
        classPrefix = "StaticRecNonTail",
        declArgsPrefix = None,
        evalFn = "rec",
        evalFnType = EvalFnType.Lambda,
        evalArgsPrefix = Some("rec")
      ) <<>>
      sourceStatic("staticRecTailCall",
        classPrefix = "StaticRecTailCall",
        declArgsPrefix = None,
        evalFn = "selfTailCall",
        evalFnType = EvalFnType.SelfTailCall,
        evalArgsPrefix = None
      ) <<>>
      sourceDynamic(isTail = false) <<>>
      sourceDynamic(isTail = true)
    }

  def sourceStatic(defName: String, classPrefix: String, declArgsPrefix: Option[String], evalFn: String, evalFnType: EvalFnType, evalArgsPrefix: Option[String]) = {
    val evalArgsPrefixStr = evalArgsPrefix.map(_ + ", ").getOrElse("")
    def eEvalArgs(argCount: Int) =
      evalArgsPrefixStr + (argCount - 1 to 0 by -1).commas(i => s"e${i}, e${i}b") + commaIf(argCount) + "r"

    def xEvalArgs(argCount: Int) =
      "rec, " + (0 until argCount).commas(i => s"x${i}, x${i}b") + commaIf(argCount) + "r"

    def renderEvalFn = evalFnType match {
      case EvalFnType.Lambda => s"Render1.render($evalFn.decompile)"
      case EvalFnType.TailCall => "\"tailCall\""
      case EvalFnType.SelfTailCall => "\"selfTailCall\""
    }

    bEq(s"def $defName(e: TermC, " + declArgsPrefix.map(_ + ", ").getOrElse("") + "args: Array[Computation]): Computation") {
      "warnAssert(stackSize(e) == args.map(_.stackSize).max," <>
        """s"stackSize: ${stackSize(e)}, args: ${args.map(_.stackSize).mkString(", ")}")""".indent <>
      switch("stackSize(e)") {
          (0 to maxInlineStack).eachNL { stackSize =>
            `case`(s"/* stackSize = */ $stackSize") {
              switch("args.length") {
                (1 to maxInlineArgs).eachNL { argCount =>
                  `case`(argCount) {
                    val className = s"${classPrefix}S${stackSize}A${argCount}"
                    (0 until argCount).each { i => s"val arg$i = args($i)" } <>
                    b(s"class $className extends Computation${stackSize}(e, ())") {
                      bEq(applySignature(stackSize)) {
                        includeIf(traceEval)(
                          s"""logFine("$className " + Render1.render(this.decompile) + ".stack(" + """
                          + includeIf(stackSize)((0 until stackSize).map(i => s"render(x$i, x${i}b)").mkString("", """ + ", " + """, " + "))
                          + """")")""".<>|
                        ) +
                        (0 until argCount).each(i => (
                          s"val e$i = "
                            + catchTC(s"arg$i(${xEvalArgs(stackSize)})")
                            + s"; val e${i}b = r.boxed")) <>
                        locally { val evalStr = s"$evalFn(${eEvalArgs(argCount)})"
                          includeIfElse(traceEval)(
                            s"""logFine("$className-> " + $renderEvalFn + "("""" +
                            (argCount - 1 to 0 by -1).map(i => s"render(e$i, e${i}b)").mkString(" + ", """ + ", " + """, " + ") +
                            """")")""" <>
                            s"""val d = $evalStr; logFine("$className-> " + render(d, r.boxed)); d""",
                            evalStr
                          )
                        }
                      }
                    } <>
                    s"new $className"
                  }
                } <<>>
                `case`("argCount") {
                  val className = s"${classPrefix}S${stackSize}AN"
                  b(s"class $className extends Computation${stackSize}(e, ())") {
                    bEq(applySignature(stackSize)) {
                      includeIf(traceEval)(
                        s"""logFine("$className " + Render1.render(this.decompile) + ".stack(" + """
                          + includeIf(stackSize)((0 until stackSize).map(i => s"render(x$i, x${i}b)").mkString("", """ + ", " + """, " + "))
                          + """")")""".<>|
                      ) +
                      "val slots = new Array[Slot](argCount)" <>
                      "var i = 0" <>
                      b("while (i < argCount)") {
                        "val slot = slots(argCount - 1 - i)" <>
                        s"slot.unboxed = " + catchTC(s"args(i)(${xEvalArgs(stackSize)})") <>
                        s"slot.boxed = r.boxed" <>
                        "i += 1"
                      } <>
                      locally {
                        val evalStr = s"$evalFn(${evalArgsPrefixStr}slots, r)"
                        includeIfElse(traceEval)(
                          s"""logFine("$className-> " + $renderEvalFn + "(" + """ +
                            """(0 until argCount).map(i => render(slots(i))).mkString(", ") + """ +
                            """")")""" <>
                            s"""val d = $evalStr; logFine("$className-> " + render(d, r.boxed)); d""",
                          evalStr
                        )
                      }
                    }
                  } <>
                  s"new $className"
                }
              }
            }
          } <<>>
          `case`("stackSize") {
            switch("args.length") {
              (1 to maxInlineArgs).eachNL { argCount =>
                `case`(s"/* argCount = */ $argCount") {
                  val className = s"${classPrefix}SNA$argCount"
                  (0 until argCount).each { i => s"val arg$i = args($i)" } <>
                  b(s"class $className extends ComputationN(stackSize, e, ())") {
                    bEq(applyNSignature) {
                      includeIf(traceEval)(
                        s"""logFine("$className " + Render1.render(this.decompile) + ".stack(" + """
                          + s"""(0 until stackSize).map(i => render(xs(i).unboxed, xs(i).boxed)).mkString(", ") + """
                          + """")")""".<>|
                      ) +
                      (0 until argCount).each { i =>
                        s"val e$i = " + catchTC(s"arg$i(rec, xs, r)") + s"; val e${i}b = r.boxed"
                      } <>
                      locally {
                        val evalStr = s"$evalFn(${eEvalArgs(argCount)})"
                        includeIfElse(traceEval)(
                          s"""logFine("$className-> " + $renderEvalFn + "("""" +
                            (argCount - 1 to 0 by -1).map(i => s"render(e$i, e${i}b)").mkString(" + ", """ + ", " + """, " + ") +
                            """")")""" <>
                            s"""val d = $evalStr; logFine("$className-> " + render(d, r.boxed)); d""",
                          evalStr
                        )
                      }

                    }
                  } <>
                  s"new $className"
                }
              } <<>>
              `case`("argCount") {
                val className = s"${classPrefix}SNAN"
                b(s"class $className extends ComputationN(stackSize, e, ())") {
                  bEq(applyNSignature) {
                    includeIf(traceEval)(
                      s"""logFine("$className " + Render1.render(this.decompile) + ".stack(" + """
                        + s"""(0 until stackSize).map(i => render(xs(i).unboxed, xs(i).boxed)).mkString(", ") + """
                        + """")")""".<>|
                    ) +
                    "val slots = new Array[Slot](argCount)" <>
                    "var i = 0" <>
                    b("while (i < argCount)") {
                      "val slot = slots(argCount - 1 - i)" <>
                        s"slot.unboxed = " + catchTC(s"args(i)(rec, xs, r)") <>
                        s"slot.boxed = r.boxed" <>
                        "i += 1"
                    } <>
                    locally {
                      val evalStr = s"$evalFn(${evalArgsPrefixStr}slots, r)"
                      includeIfElse(traceEval)(
                        s"""logFine("$className-> " + $renderEvalFn + "(" + slots.map(render).mkString(", ") + ")")""" <>
                        s"""val d = $evalStr; logFine("$className-> " + render(d, r.boxed)); d""",
                        evalStr
                      )
                    }
                  }
                } <>
                s"new $className"
              }
            }
          }
        }
    }
  }

  def sourceDynamic(isTail: Boolean): String = {
    val emptyOrNon = if (isTail) "" else "Non"
    bEq(s"def dynamic${emptyOrNon}TailCall(e: TermC, mkFn: Computation, args: Array[Computation]): Computation") {
      "warnAssert(stackSize(e) == (mkFn.stackSize max args.map(_.stackSize).max), " <>
        """e.toString + "\n" +
          |s"stackSize(${e.annotation}): ${stackSize(e)}\n" +
          |s"mkFn (" + mkFn.stackSize + "):\n" +
          |  org.unisonweb.Render.renderIndent(mkFn.decompile) + "\n" +
          |s"args (" + args.map(_.stackSize).mkString(", ") + "):\n" +
          |  args.map(arg => org.unisonweb.Render.renderIndent(arg.decompile)).mkString(",\n")
          |""".stripMargin.indent <>
      ")" <>
      switch("stackSize(e)") {
        (0 to maxInlineStack).eachNL { stackSize =>
          `case`(s"/* stackSize = */ $stackSize") {
            switch("args.length") {
              (1 to maxInlineArgs).eachNL { argCount =>
                `case`(s"/* argCount = */ $argCount") {
                  val className = s"Dynamic${emptyOrNon}TailCallS${stackSize}A${argCount}"
                  b(s"class $className extends Computation$stackSize(e, ())") {
                    (0 until argCount).each(j => s"val arg$j = args($j)") <>
                    bEq(applySignature(stackSize)) {
                      includeIf(traceEval)(
                        s"""logFine("$className " + Render1.render(this.decompile) + ".stack(" + """
                          + includeIf(stackSize)((0 until stackSize).map(i => s"render(x$i, x${i}b)").mkString("", """ + ", " + """, " + "))
                          + """")")""".<>|
                      ) +
                      s"val lambda = ${evalBoxed(stackSize, "mkFn")}.asInstanceOf[Lambda]" <>
                      (0 until argCount).each { j => s"val arg${j}r = " + eval(stackSize, s"arg$j") + s"; val arg${j}rb = r.boxed" } <>
                      locally {
                        val (evalStr, renderEvalFn) =
                          if (isTail)
                            "tailCall(lambda, " + (argCount-1 to 0 by -1).commas(j => s"arg${j}r, arg${j}rb") + commaIf(argCount) + "r)" ->
                            """"tailCall " +  Render1.render(lambda.decompile)"""
                          else
                            "lambda(lambda, " + (argCount-1 to 0 by -1).commas(j => s"arg${j}r, arg${j}rb") + commaIf(argCount) + "r)" ->
                            "Render1.render(lambda.decompile)"
                        includeIfElse(traceEval)(
                          s"""logFine("$className-> " + $renderEvalFn + "("""" +
                            (argCount - 1 to 0 by -1).map(i => s"render(arg${i}r, arg${i}rb)").mkString(" + ", """ + ", " + """, " + ") +
                            """")")""" <>
                            s"""val d = $evalStr; logFine("$className-> " + render(d, r.boxed)); d""",
                          evalStr
                        )
                      }
                    }
                  } <>
                  s"new $className"
                }
              } <<>>
              `case`("argCount") {
                val className = s"Dynamic${emptyOrNon}TailCallS${stackSize}AN"
                b(s"class $className extends Computation$stackSize(e, ())") {
                  bEq(applySignature(stackSize)) {
                    includeIf(traceEval)(
                      s"""logFine("$className " + Render1.render(this.decompile) + ".stack(" + """
                        + includeIf(stackSize)((0 until stackSize).map(i => s"render(x$i, x${i}b)").mkString("", """ + ", " + """, " + "))
                        + """")")""".<>|
                    ) +
                    "val argsr = new Array[Slot](argCount)" <>
                    s"val lambda = ${evalBoxed(stackSize, "mkFn")}.asInstanceOf[Lambda]" <>
                    "var k = 0" <>
                    b("while (k < argCount)") {
                      "argsr(argCount - 1 - k) = new Slot(" + eval(stackSize, "args(k)") + ", r.boxed)" <>
                      "k += 1"
                    } <>
                    locally {
                      val (evalStr, renderEvalFn) =
                        if (isTail)
                          "tailCall(lambda, argsr, r)" ->
                          """"tailCall " +  Render1.render(lambda.decompile)"""
                        else
                          "lambda(lambda, argsr, r)" ->
                          "Render1.render(lambda.decompile)"
                      includeIfElse(traceEval)(
                        s"""logFine("$className-> " + $renderEvalFn + "(" + argsr.map(render).mkString(", ") + ")")""" <>
                        s"""val d = $evalStr; logFine("$className-> " + render(d, r.boxed)); d""",
                        evalStr
                      )
                    }
                  }
                } <>
                s"new $className"
              }
            }
          }
        } <<>>
        `case`("stackSize") {
          switch("args.length") {
            (1 to maxInlineArgs).eachNL { argCount =>
              `case`(s"/* argCount = */ $argCount") {
                val className = s"Dynamic${emptyOrNon}TailCallSNA$argCount"
                b(s"class $className extends ComputationN(stackSize, e, ())") {
                  (0 until argCount).each(j => s"val arg$j = args($j)") <>
                  bEq(applyNSignature) {
                    includeIf(traceEval)(
                      s"""logFine("$className " + Render1.render(this.decompile) + ".stack(" + """
                        + s"""(0 until stackSize).map(i => render(xs(i).unboxed, xs(i).boxed)).mkString(", ") + """
                        + """")")""".<>|
                    ) +
                    s"val lambda = ${evalNBoxed("mkFn")}.asInstanceOf[Lambda]" <>
                    (0 until argCount).each( j => s"val arg${j}r = " + evalN(s"arg$j") + s"; val arg${j}rb = r.boxed" ) <>
                    (if (!isTail) s"lambda(lambda, " + ((argCount-1) to 0 by -1).commas(j => s"arg${j}r, arg${j}rb") + commaIf(argCount) + "r)"
                    else s"tailCall(lambda, " + ((argCount-1) to 0 by -1).commas(j => s"arg${j}r, arg${j}rb") + commaIf(argCount) + "r)")
                  }
                } <>
                s"new $className"
              }
            } <<>>
            `case`("argCount") {
              val className = s"Dynamic${emptyOrNon}TailCallSNAM"
              b(s"class $className extends ComputationN(argCount, e, ())") {
                bEq(applyNSignature) {
                  includeIf(traceEval)(
                    s"""logFine("$className " + Render1.render(this.decompile) + ".stack(" + """
                      + s"""(0 until stackSize).map(i => render(xs(i).unboxed, xs(i).boxed)).mkString(", ") + """
                      + """")")""".<>|
                  ) +
                  "val argsr = new Array[Slot](argCount)" <>
                  s"val lambda = ${evalNBoxed("mkFn")}.asInstanceOf[Lambda]" <>
                  "var k = 0" <>
                  b("while (k < argCount)") {
                    "argsr(argCount - 1 - k) = new Slot(" + evalN("args(k)") + ", r.boxed)" <>
                    "k += 1"
                  } <>
                  (if (!isTail) "lambda(lambda, argsr, r)"
                  else "tailCall(lambda, argsr, r)")
                }
              } <>
              s"new $className"
            }
          }
        }
      }
    }
  }
}
