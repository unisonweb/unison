package org.unisonweb.codegeneration

object CompileLetRecGenerator extends OneFileGenerator("CompileLetRec.scala") {

  private def loopXArgs(i: Int) = (0 until i).commas(i => s"r.x$i, r.x${i}b")

  private def loopXsArgs(i: Int): String = loopXsArgs(0, i)

  private def loopXsArgs(i: Int, j: Int): String =
    (i until j).commas(i => s"r.xs($i).unboxed, r.xs($i).boxed")

  private def tryStep(argCount: Int) =
    s"try step(step, ${xArgs(argCount)}, r)" <>
      b("catch") {
        `case`("SelfTailCall") {
          b("while(true)") {
            val xArgCount = argCount min maxInlineTC
            val args = commaIf(xArgCount) + loopXArgs(xArgCount) + commaIf(argCount > maxInlineTC) + loopXsArgs(maxInlineTC, argCount)
            s"try return step(step$args, r)" <>
            "catch { case SelfTailCall => }"
          } <>
          """sys.error("reached unreachable code")"""
        }
      }

  private def tryStepN =
    s"try step(step, xs, r)" <>
    b("catch") {
      `case`("SelfTailCall") {
        b("while(true)") {
          s"try return step(step, r.xs, r)" <>
          "catch { case SelfTailCall => }"
        } <>
        """sys.error("reached unreachable code")"""
      }
    }



  def source =
    "package org.unisonweb.compilation" <>
    "" <>
    "import org.unisonweb.Term" <>
    "import org.unisonweb.Term.Name" <>
    "" <>
    b("trait CompileLetRec") (
      bEq("def compileLetRec(e: TermC, bindings: Array[(Name, Computation)], body: Computation): Computation") {
        switch("stackSize(e)") {
           (0 to maxInlineStack).eachNL { stackSize =>
             `case`(s"/* stackSize = */ $stackSize") {
               switch("bindings.length") {
                 (1 to maxInlineArgs).eachNL { argCount =>
                   `case`(s"/* argCount = */ $argCount") {
                     b(s"class LetRecS${stackSize}A${argCount} extends Computation$stackSize(e, ())") {
                       (1 to argCount).each { k => s"val b$k = bindings(${k-1})._2" } <>
                       (1 to argCount).each { k => s"val b${k}n = bindings(${k-1})._1" } <>
                       bEq(applySignature(stackSize)) {
                         val bArgs =
                           if (stackSize + argCount <= maxInlineStack)
                             (argCount to 1 by -1).commas(l => s"0.0, b${l}r") + commaIf(stackSize) +
                               (0 until stackSize).commas(l => s"x$l, x${l}b")
                           else
                             "Array(" +
                               (argCount to 1 by -1).commas(l => s"Slot(0.0, b${l}r)") + commaIf(stackSize) +
                               (0 until stackSize).commas(l => s"Slot(x$l, x${l}b)") +
                               ")"
                         (1 to argCount).each { k => s"val b${k}r = Ref(b${k}n)" } <>
                         (1 to argCount).each { k => s"b${k}r.value = Value(" + catchTC(s"b$k(rec, $bArgs, r)") + ", r.boxed)" } <>
                         s"body(rec, $bArgs, r)"
                       }
                     } <>
                     s"new LetRecS${stackSize}A${argCount}"
                   }
                 } <<>>
                 `case`("/* argCount = */ m") {
                   b(s"class LetRecS${stackSize}AM extends Computation$stackSize(e, ())") {
                     bEq(applySignature(stackSize)) {
                       "val refs = bindings.map { case (name, _) => Ref(name) }" <>
                         "val xs = Array[Slot](" + (0 until stackSize).commas(k => s"Slot(x$k,x${k}b)") + ")" <>
                         "val slots = refs.view.map(r => Slot(0.0, r)).reverse.toArray ++ xs" <>
                         "var i = 0" <>
                         b("while (i < bindings.length)") {
                           "refs(i).value = Value(" + catchTC("bindings(i)._2(rec, slots, r)") + ", r.boxed)" <>
                             "i += 1"
                         } <>
                         "body(rec, slots, r)"
                     }
                   } <>
                     s"new LetRecS${stackSize}AM"
                 }
               }
             }
           } <<>>
           `case`("/* stackSize = */ n") {
             b(s"class LetRecSN extends ComputationN(n, e, ())") {
               bEq(applyNSignature) {
                 "val refs = bindings.map { case (name, _) => Ref(name) }" <>
                 "val slots = refs.view.map(r => Slot(0.0, r)).reverse.toArray ++ xs" <>
                 "var i = 0" <>
                 b("while (i < bindings.length)") {
                   "refs(i).value = Value(" + catchTC("bindings(i)._2(rec, slots, r)") + ", r.boxed)" <>
                     "i += 1"
                 } <>
                 "body(rec, slots, r)"
               }
             } <>
             "new LetRecSN"
           }
        }
      } <<>>
      bEq("def compileLetRecBindings(currentRec: CurrentRec, bindings: Array[(Name, TermC)], compile: CurrentRec => TermC => Computation): Array[Computation]") {
        b("bindings.map") {
          `case`("(name, l@Term.Lam(names, body))") {
            "val currentRec = CurrentRec(name, names.size).shadow(names)" <>
            "val mkLambda = compile(currentRec)(l)" <>
            "def decompile = mkLambda.decompile" <>
            b("if (hasTailRecursiveCall(currentRec, body))") {
              switch("stackSize(l)") {
                (0 to maxInlineStack).eachNL { stackSize =>
                  `case`(stackSize) {
                    switch("names.size") {
                      (1 to maxInlineArgs).eachNL { argCount =>
                        `case`(argCount) {
                          val className = s"LetRecBindingS${stackSize}A$argCount"
                          b(s"class $className extends Computation$stackSize(decompile)") {
                            bEq(applySignature(stackSize)) {
                              s"val step = { mkLambda(null, ${xArgs(stackSize) + commaIf(stackSize)}r); r.boxed.asInstanceOf[Lambda] }" <<>>
                              b(s"class Body extends Computation$argCount(step.decompile)") {
                                indentEqExpr("override " + applySignature(argCount)) {
                                  tryStep(argCount)
                                }
                              } <<>>
                              s"r.boxed = new Lambda$argCount(${(0 until argCount).commas(i => s"names($i)")}, new Body, step.decompile)" +
                                includeIf(argCount > 1)("(body, compile(currentRec))") <>
                              "0.0"
                            }
                          } <>
                          s"new $className"
                        }
                      } <<>>
                      `case`(s"argCount /* >$maxInlineArgs */") {
                        val className = s"LetRecBindingS${stackSize}AN"
                        b(s"class $className extends Computation$stackSize(decompile)") {
                          bEq(applySignature(stackSize)) (
                            s"val step = { mkLambda(null, ${xArgs(stackSize) + commaIf(stackSize)}r); r.boxed.asInstanceOf[Lambda] }" <<>>

                            b("class Body extends ComputationN(argCount, step.decompile)") {
                              bEq(applyNSignature) {
                                tryStepN
                              }
                            } <<>>

                            s"r.boxed = new LambdaN(names.toArray, new Body, step.decompile)(body, compile(currentRec))" <>
                            "0.0"
                          )
                        } <>
                        s"new $className"
                      }
                    }
                  }
                } <<>>
                  `case`(s"stackSize /* >$maxInlineStack */") {
                    switch("names.size") {
                      (1 to maxInlineArgs).eachNL { argCount =>
                        `case`(argCount) {
                          val className = s"LetRecBindingSNA$argCount"
                          b(s"class $className extends ComputationN(stackSize, decompile)") (
                            bEq(applyNSignature) (
                              s"val step = { mkLambda(null, xs, r); r.boxed.asInstanceOf[Lambda] }" <<>>
                              b(s"class Body extends Computation$argCount(step.decompile)") {
                                bEq(applySignature(argCount)) {
                                  tryStep(argCount)
                                }
                              } <<>>

                              s"r.boxed = new LambdaN(names.toArray, new Body, step.decompile)(body, compile(currentRec))" <>
                              "0.0"
                            )
                          ) <>
                          s"new $className"
                        }
                      } <<>>
                      `case`(s"argCount /* >$maxInlineArgs */") {
                        val className = "LetRecBindingSNAM"
                        b(s"class $className extends ComputationN(stackSize, decompile)") (
                          bEq(applyNSignature) (
                            s"val step = { mkLambda(null, xs, r); r.boxed.asInstanceOf[Lambda] }" <<>>

                            b("class Body extends ComputationN(argCount, step.decompile)") {
                              bEq(applyNSignature) {
                                tryStepN
                              }
                            } <<>>

                            s"r.boxed = new LambdaN(names.toArray, new Body, step.decompile)(body, compile(currentRec))" <>
                            "0.0"

                          )
                        ) <>
                        s"new $className"
                      }
                    }

                  }
              }
            } <>
            "else mkLambda"
          } <<>>
          `case`("(_, b) /* not a lambda, shouldn't contain recursive call */") {
            "compile(currentRec)(b)"
          }
        }
      }
    )
}
