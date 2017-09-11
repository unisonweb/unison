package org.unisonweb.codegeneration

object CompileLetRecGenerator extends OneFileGenerator("CompileLetRec.scala") {

  def loopSignature(i: Int) = "@annotation.tailrec\ndef loop(" + (0 until i).commas(i => s"x$i: D, x${i}b: V") + ", r: R): D"

  // loop signature maxes out at maxInlineTC to minimize repacking for the tail-recursive call, rather than the initial call
  def loopSignatureN = (
    "def loop("
      + (0 until maxInlineTC).commas(i => s"x$i: D, x${i}b: V") + commaIf(maxInlineTC)
      + "xs: Array[Slot], r: R): D"
    )

  def loopXArgs(i: Int) = (0 until i).commas(i => s"e.x$i, e.x${i}b")

  def source =
    "package org.unisonweb.compilation" <>
    "" <>
    "import org.unisonweb.Term" <>
    "import org.unisonweb.Term.Name" <>
    "" <>
    b("trait CompileLetRec") (
      bEq("def compileLetRec(e: TermC, bindings: Array[Computation], body: Computation): Computation") {
        switch("stackSize(e)") {
           (0 to maxInlineStack).eachNL { stackSize =>
             `case`(s"/* stackSize = */ $stackSize") {
               switch("bindings.length") {
                 (1 to maxInlineArgs).eachNL { argCount =>
                   `case`(s"/* argCount = */ $argCount") {
                     b(s"class LetRecS${stackSize}A${argCount} extends Computation$stackSize(e, ())") {
                       (1 to argCount).each { k => s"val b$k = bindings(${k-1})" } <>
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
                         "val " + (1 to argCount).commas { k => s"b${k}r" } + " = Ref()" <>
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
                       "val refs = Array.fill(m)(Ref())" <>
                         "val xs = Array[Slot](" + (0 until stackSize).commas(k => s"Slot(x$k,x${k}b)") + ")" <>
                         "val slots = refs.view.map(r => Slot(0.0, r)).reverse.toArray ++ xs" <>
                         "var i = 0" <>
                         b("while (i < bindings.length)") {
                           "refs(i).value = Value(" + catchTC("bindings(i)(rec, slots, r)") + ", r.boxed)" <>
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
                 "val refs = Array.fill(bindings.length)(Ref())" <>
                 "val slots = refs.view.map(r => Slot(0.0, r)).reverse.toArray ++ xs" <>
                 "var i = 0" <>
                 b("while (i < bindings.length)") {
                   "refs(i).value = Value(" + catchTC("bindings(i)(rec, slots, r)") + ", r.boxed)" <>
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
                              (if (argCount <= maxInlineTC)
                                indentEqExpr(loopSignature(argCount)) {
                                  s"try step(step, ${xArgs(argCount)}, r)" <>
                                  s"catch { case e: SelfCall => loop(${loopXArgs(argCount)}, r) }"
                                }
                              else // argCount > maxInlineTC
                                indentEqExpr(loopSignatureN) {
                                  s"try step(step, ${xArgs(maxInlineTC)}, ${(0 until argCount-maxInlineTC).commas(i => s"xs($i).unboxed, xs($i).boxed")}, r)" <>
                                  s"catch { case e: SelfCall => loop(${loopXArgs(maxInlineTC)}, e.args, r) }"
                                }
                              ) <<>>
                              b(s"class Body extends Computation$argCount(step.decompile)") {
                                indentEqExpr("override " + applySignature(argCount)) {
                                  if (argCount <= maxInlineTC)
                                    s"loop(${xArgs(argCount)}, r)"
                                  else
                                    s"loop(${xArgs(maxInlineTC)}, Array(${(maxInlineTC until argCount).commas(slot)}), r)"
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
                      `case`("argCount") {
                        val className = s"LetRecBindingS${stackSize}AN"
                        s"??? // new $className // todo argCount > $maxInlineArgs"
                      }
                    }
                  }
                } <<>>
                  `case`("stackSize") {
                    switch("names.size") {
                      (1 to maxInlineArgs).eachNL { argCount =>
                        s"??? // todo stackSize > $maxInlineStack, argCount = $argCount"
                      }
                      `case`("argCount") {
                        s"??? // todo stackSize > $maxInlineStack, argCount > $maxInlineArgs"
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
