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
        "@inline def eUnC = unTermC(e)" <>
        "@inline def bodyUnC = unTermC(body)" <>
        // todo: handle free vars
        "def makeClosedLambda(e: Term, names: List[Name], compiledBody: Computation)(body: Term): Lambda = " + `match`("names") {
          (1 to maxInlineArgs).foldRight(caseInline("_ :: _")("new LambdaN(names.toArray, e, compiledBody)(body, compile(currentRec))")) {
            case (1, rest) => caseInline("name1 :: tl") {
              `match`("tl") {
                "case Nil => new Lambda1(name1, e, compiledBody)" <>
                  rest
              }
            }
            case (i, rest) => caseInline(s"name$i :: tl") {
              `match`("tl") {
                s"case Nil => new Lambda$i(" + (1 to i).commas("name" + _) + ", e, compiledBody)(body, compile(currentRec))" <>
                  rest
              }
            }
          } <>
          "case Nil => sys.error(\"impossible, lambdas need parameters\")"
        } <>
        // grab all the free variables referenced by the body of the lambda (not bound by the lambda itself)
        "val shadowedRec = currentRec.shadow(names)" <>
        "val fv: Set[Name] = freeVars(e)" <>
        "if (fv.isEmpty) Return(makeClosedLambda(eUnC, names, compile(shadowedRec)(bodyUnC))(bodyUnC))(eUnC)" <>
        b("else") {
          // get them off the stack when you build the lambda
          //  (compile/get all those `Var`s)
          //  take those Values,
          // decompile them,
          // substitute them into the body of the lambda, being careful about name clashes
          // now the lambda has no more free variables; good to compile with happy path
          switch("stackSize(e)") {
            caseInline(0)("throw new Exception(\"Should have some free vars on the stack!\")") <>
            (1 to maxInlineStack).each { stackSize =>
              `case`(stackSize) {
                val className = s"BindLambdaS${stackSize}"
                b(s"class $className extends Computation${stackSize}(e,())") {
                  bEq(applySignature(stackSize)) {
                    "val compiledVars: Map[Name, Term] =" <>
                      s"fv.map { name => name -> Term.Compiled(Value(${
                        eval(stackSize, "compileVar(currentRec, name, e)")
                      }, r.boxed)) }.toMap".indent <<>>
                    "val body2 = ABT.substs(compiledVars)(bodyUnC)" <>
                    "val compiledBody2 = compile(shadowedRec)(body2)" <>
                    "val lam2 = Term.Lam(names: _*)(body2)" <>
                    "r.boxed = makeClosedLambda(lam2, names, compiledBody2)(lam2)" <>
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
                      evalN("compileVar(currentRec, name, e)")
                    }, r.boxed)) }.toMap".indent <<>>
                  "val body2 = ABT.substs(compiledVars)(bodyUnC)" <>
                  "val compiledBody2 = compile(shadowedRec)(body2)" <>
                  "val lam2 = Term.Lam(names: _*)(body2)" <>
                  "r.boxed = makeClosedLambda(lam2, names, compiledBody2)(lam2)" <>
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

