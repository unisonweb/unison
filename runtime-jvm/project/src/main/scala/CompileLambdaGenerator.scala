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
      bEq("def compileLambda(e: TermC, names: List[Name], body: TermC, currentRec: CurrentRec, compile: CurrentRec => TermC => Computation): Computation") {
        "@inline def decompiledLam = unTermC(e)" <<>>
        "def makeClosedLambda(decompiledLam: Term, names: List[Name], body: TermC, compiledBody: Computation): Lambda = " + `match`("names") {
          (1 to maxInlineArgs).foldRight(caseInline("_ :: _")("new LambdaN(names.toArray, compiledBody, decompiledLam)(body, compile(currentRec))")) {
            case (1, rest) => caseInline("name1 :: tl") {
              `match`("tl") {
                "case Nil => new Lambda1(name1, compiledBody, decompiledLam)" <>
                  rest
              }
            }
            case (i, rest) => caseInline(s"name$i :: tl") {
              `match`("tl") {
                s"case Nil => new Lambda$i(" + (1 to i).commas("name" + _) + ", compiledBody, decompiledLam)(body, compile(currentRec))" <>
                  rest
              }
            }
          } <>
          "case Nil => sys.error(\"impossible, lambdas need parameters\")"
        } <<>>
        // grab all the free variables referenced by the body of the lambda (not bound by the lambda itself)
        "val shadowedRec = currentRec.shadow(names)" <>
        "val fv: Set[Name] = freeVars(e) -- recVars(e).get" <>
        "val rv: Set[Name] = freeVars(e) intersect recVars(e).get" <>
        // get them off the stack when you build the lambda
        //  (compile/get all those `Var`s)
        //  take those Values,
        // decompile them,
        // substitute them into the body of the lambda, being careful about name clashes
        // now the lambda has no more free variables; good to compile with happy path
        // ---
        // "keep track of recursive vars"
        // set of names, shadow each time
        // or: add another annotation to the tree: TermC to (FreeVars, BoundVars, RecVars)
        // currently, when you compile a lambda with free vars, we grab the fvs from their env and subst into the lambda
        // but we are eager about how we do it: if the free var is a ref, we resolve the ref eagerly and subst the value in
        // instead, don't resolve that ref :D substitute the Return(ref) itself into the body
        switch("stackSize(e)") {
          `case`(0) {
            "assert(fv.isEmpty)" <>
            "Return(makeClosedLambda(decompiledLam, names, body, compile(shadowedRec)(body)))(decompiledLam)"
          } <>
          (1 to maxInlineStack).eachNL { stackSize =>
            `case`(stackSize) {
              val className = s"BindLambdaS${stackSize}"
              b(s"class $className extends Computation${stackSize}(e,())") {
                bEq(applySignature(stackSize)) {
                  "val compiledFreeVars: Map[Name, Term] = " + b("fv.map") {
                    `case`("name") {
                      "val compiledVar = compileVar(currentRec, name, env(e))" <>
                      "val evaluatedVar = " + eval(stackSize, "compiledVar") <>
                      "val value = Value(evaluatedVar, r.boxed)" <>
                      "(name, Term.Compiled(value))"
                    }
                  } + ".toMap" <<>>
                  "val compiledRecVars: Map[Name, Term] = " + b("rv.map") {
                    `case`("name") {
                      "val compiledVar = compileRefVar(currentRec, name, env(e))" <>
                      "val evaluatedVar = " + eval(stackSize, "compiledVar") <>
                      "val value = Value(evaluatedVar, r.boxed)" <>
                      "(name, Term.Compiled(value))"
                    }
                  } + ".toMap" <<>>
                  "val lam2 = Term.Lam(names: _*)(body = ABT.substs(compiledFreeVars ++ compiledRecVars)(unTermC(body)))" <>
                  "r.boxed = compile(currentRec)(checkedAnnotateBound(lam2)) match {" <>
                  "  case Return(v) => v" <>
                  "  case _ => sys.error(\"compiling a lambda with no free vars should always produce a Return.\")" <>
                  "}" <>
                  "0.0"
                }
              } <>
              s"new $className"
            }
          } <<>>
          `case`("stackSize") {
            val className = s"BindLambdaSN"
            b(s"class $className extends ComputationN(stackSize,e,())") {
              bEq(applyNSignature) {
                "val compiledFreeVars: Map[Name, Term] =" <>
                  s"fv.map { name => name -> Term.Compiled(Value(${evalN("compileVar(currentRec, name, env(e))")}, r.boxed)) }.toMap".indent <<>>
                "val compiledRecVars: Map[Name, Term] =" <>
                  s"rv.map { name => name -> Term.Compiled(Value(${evalN("compileRefVar(currentRec, name, env(e))")}, r.boxed)) }.toMap".indent <<>>
                "// System.out.println(\"[debug] compiled vars:\\n\" + fv.mkString(\"  \", \"\\n  \", \"\\n\"))"<>
                "val lam2 = Term.Lam(names: _*)(body = ABT.substs(compiledFreeVars ++ compiledRecVars)(unTermC(body)))" <>
                "r.boxed = compile(currentRec)(checkedAnnotateBound(lam2)) match {" <>
                "  case Return(v) => v" <>
                "  case _ => sys.error(\"compiling a lambda with no free vars should always produce a Return.\")" <>
                "}" <>
                "0.0"
              }
            } <>
            s"new $className"
          }
        }
      }
    }
}

