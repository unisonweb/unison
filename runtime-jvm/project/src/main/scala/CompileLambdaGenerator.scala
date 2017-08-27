package org.unisonweb.codegeneration

object CompileLambdaGenerator extends OneFileGenerator("CompileLambda.scala") {
  def source: String =
    "package org.unisonweb.compilation" <>
    "" <>
      "import org.unisonweb.ABT" <>
      "import org.unisonweb.Term" <>
      "import org.unisonweb.Term.{Term, Name}" <>
    "" <>
    b("trait CompileLambda") {
      bEq("def compileLambda(e: TermC, names: List[Name], body: TermC, currentRec: CurrentRec, compile: CurrentRec => Term => Computation): Computation") {
        "@inline def decompiledLam = unTermC(e)" <>
        "@inline def decompiledBody = unTermC(body)" <<>>
        "def makeClosedLambda(decompiledLam: Term, names: List[Name], decompiledBody: Term, compiledBody: Computation): Lambda = " + `match`("names") {
          (1 to maxInlineArgs).foldRight(caseInline("_ :: _")("new LambdaN(names.toArray, compiledBody, decompiledLam)(decompiledBody, compile(currentRec))")) {
            case (1, rest) => caseInline("name1 :: tl") {
              `match`("tl") {
                "case Nil => new Lambda1(name1, compiledBody, decompiledLam)" <>
                  rest
              }
            }
            case (i, rest) => caseInline(s"name$i :: tl") {
              `match`("tl") {
                s"case Nil => new Lambda$i(" + (1 to i).commas("name" + _) + ", compiledBody, decompiledLam)(decompiledBody, compile(currentRec))" <>
                  rest
              }
            }
          } <>
          "case Nil => sys.error(\"impossible, lambdas need parameters\")"
        } <<>>
        // grab all the free variables referenced by the body of the lambda (not bound by the lambda itself)
        "val shadowedRec = currentRec.shadow(names)" <>
        "val fv: Set[Name] = freeVars(body)" <>
        "if (fv.isEmpty) Return(makeClosedLambda(decompiledLam, names, decompiledBody, compile(shadowedRec)(decompiledBody)))(decompiledLam)" <>
        b("else") {
          // get them off the stack when you build the lambda
          //  (compile/get all those `Var`s)
          //  take those Values,
          // decompile them,
          // substitute them into the body of the lambda, being careful about name clashes
          // now the lambda has no more free variables; good to compile with happy path
          switch("stackSize(body)") {
            caseInline(0)("throw new Exception(\"Should have some free vars on the stack!\")") <>
            (1 to maxInlineStack).each { stackSize =>
              `case`(stackSize) {
                val className = s"BindLambdaS${stackSize}"
                b(s"class $className extends Computation${stackSize}(e,())") {
                  bEq(applySignature(stackSize)) {
                    "val compiledVars: Map[Name, Term] = " + b("fv.map") {
                      `case`("name") {
                        "val compiledVar = compileVar(currentRec, name, body)" <>
                        "val evaluatedVar = " + eval(stackSize, "compiledVar") <>
                        "val value = Value(evaluatedVar, r.boxed)" <>
                        "(name, Term.Compiled(value))"
                      }
                    } + ".toMap" <<>>
                    "val decompiledBody2 = ABT.substs(compiledVars)(decompiledBody)" <>
                    "val compiledBody2 = compile(shadowedRec)(decompiledBody2)" <>
                    "val decompiledLam2 = Term.Lam(names: _*)(decompiledBody2)" <>
                    "r.boxed = makeClosedLambda(decompiledLam2, names, decompiledBody2, compiledBody2)" <>
                    "0.0"
                  }
                } <>
                s"new $className"
              }
            } <>
            `case`("stackSize") {
              val className = s"BindLambdaSN"
              b(s"class $className extends ComputationN(stackSize,e,())") {
                bEq(applyNSignature) {
                  "val compiledVars: Map[Name, Term] =" <>
                    s"fv.map { name => name -> Term.Compiled(Value(${
                      evalN("compileVar(currentRec, name, body)")
                    }, r.boxed)) }.toMap".indent <<>>
                  "// System.out.println(\"[debug] compiled vars:\\n\" + fv.mkString(\"  \", \"\\n  \", \"\\n\"))"<>
                  "val decompiledBody2 = ABT.substs(compiledVars)(decompiledBody)" <>
                  "val compiledBody2 = compile(shadowedRec)(decompiledBody2)" <>
                  "val decompiledLam2 = Term.Lam(names: _*)(decompiledBody2)" <>
                  "r.boxed = makeClosedLambda(decompiledLam2, names, decompiledBody2, compiledBody2)" <>
                  "0.0"
                }
              } <>
              s"new $className"
            }
          }
        }
      }
    }
}

