package org.unisonweb.codegeneration

object CompileLambdaGenerator extends OneFileGenerator("CompileLambda.scala") {

  def source: String =
    "package org.unisonweb.compilation" <>
    "" <>
    "import org.unisonweb.Term.{Name, Var}" <>
    "" <>
    "trait CompileLambda " + {
      "def compileLambda(" <>
      "  builtins: String => Rt, e: TermC, boundByCurrentLambda0: BoundByCurrentLambda," <>
      "  recursiveVars0: RecursiveVars, currentRec0: CurrentRec)(names: List[Name], body: TermC): Rt = " + {
        "val boundByCurrentLambda = BoundByCurrentLambda(names.toSet)" <>
        "val recursiveVars = recursiveVars0 -- names // parameter names shadow any external recursive vars" <>
        "val currentRec = currentRec0.shadow(names) // parameter names shadow any enclosing recursive function" <>
        "def makeCompiledBody = compile(builtins, body, boundByCurrentLambda, recursiveVars, currentRec, IsTail)" <>
        "lazy val eUnC = unTermC(e)" <>
        "lazy val bodyUnC = unTermC(body)" <>
        "def makeLambda = names match " + {
            (1 to N).foldRight("case _ :: _ => new LambdaN(names.toArray, eUnC, bodyUnC, makeCompiledBody, builtins)") {
              case (1, rest) => "case name1 :: tl => tl match " + {
                "case Nil => new Lambda1(name1, eUnC, makeCompiledBody)" <>
                rest
              }.b
              case (i, rest) => s"case name$i :: tl => tl match " + {
                s"case Nil => new Lambda$i(" + (1 to i).commas("name" + _) + ", eUnC, bodyUnC, makeCompiledBody, builtins)" <>
                  rest
              }.b
            } <>
            "case Nil => sys.error(\"impossible, lambdas need parameters\")"
        }.b <>
        "" <>
        "if (freeVars(e).isEmpty) makeLambda" <>
        "else " + {
          "val locallyBound = freeVars(body).filter(v => !recursiveVars.contains(v))" <>
          "(arity(locallyBound, env(e)) : @annotation.switch) match " + {
            "case 0 => makeLambda" <>
            (if (N >= 1)
              "case 1 =>" <> {
                "class Lambda_1 extends Arity1(e,()) with AccumulateBound " + {
                  "val v = locallyBound.toList.head" <>
                  "val compiledVar = lookupVar(0, v, Var(v))" <>
                  "def apply(rec: Rt, x1: D, x1b: Rt, r: R) = " + {
                    "val lam = makeLambda" <>
                    "lam.bind(bound + (v -> r.toRuntime(compiledVar(rec, x1, x1b, r))))" <>
                    "r.boxed = lam" <>
                    "0.0"
                  }.b
                }.b <>
                "new Lambda_1"
              }
            else "").indent <>
            (2 to N).each { i =>
              s"case $i =>" <> {
                s"class Lambda_$i extends Arity$i(e,()) with AccumulateBound " + {
                  "val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), v, Var(v))) }.toArray" <>
                  applySignature(i) + " = " + {
                    "var i = 0; var rts = Map[Name,Rt]()" <>
                    "while (i < vars.length) " + {
                      s"rts = rts + (vars(i)._1 -> r.toRuntime(vars(i)._2.${tailEval(i, "apply")}))" <>
                      "i += 1"
                    }.b <>
                    "val lam = makeLambda" <>
                    "lam.bind(bound ++ rts)" <>
                    "r.boxed = lam" <>
                    "0.0"
                  }.b
                }.b <>
                s"new Lambda_$i"
              }.indent
            } <>
            "case n =>" <> {
              "class Lambda_N extends ArityN(n,e,()) with AccumulateBound " + {
                "val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), v, Var(v))) }.toArray" <>
                "def apply(rec: Rt, args: Array[Slot], r: R) = " + {
                  "var i = 0; var rts = Map[Name,Rt]()" <>
                  "while (i < vars.length) " + {
                    "rts = rts + (vars(i)._1 -> r.toRuntime(vars(i)._2.apply(rec,args, r)))" <>
                    "i += 1"
                  }.b <>
                  "val lam = makeLambda" <>
                  "lam.bind(bound ++ rts)" <>
                  "r.boxed = lam" <>
                  "0.0"
                }.b
              }.b <>
              "new Lambda_N"
            }.indent
          }.b
        }.b
      }.b
    }.b
}
