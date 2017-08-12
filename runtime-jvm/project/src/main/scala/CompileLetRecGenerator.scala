package org.unisonweb.codegeneration

object CompileLetRecGenerator extends OneFileGenerator("CompileLetRec.scala") {
  def source: String =
    "package org.unisonweb.compilation" <>
    "" <>
    "import org.unisonweb.Term.Name" <>
    "" <>
    "trait CompileLetRec " + {
      "def compileLetRec(builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]]," <>
        "recursiveVars: Set[Name], currentRec: Option[(Name,Arity)], isTail: Boolean".indent <>
        ")(bindings: List[(Name,TermC)], body: TermC): Rt = ".indent + {
          // ex:
          //   let rec
          //     blah = 42
          //     let rec
          //       ping x = pong (x + 1)
          //       pong x = ping (x - 1)
          //       ping blah
          // ping = (let rec ping = ...; pong = ...; ping)
          // todo: am hazy on how we are using recursive vars to decompile
          "val names: List[Name] = bindings.map(_._1)" <>
          "val recursiveVars2: Set[Name] = recursiveVars ++ names" <>
          "// todo: when is boundByCurrentLambda ever None?" <>
          "val boundByCurrentLambda2: Option[Set[Name]] = boundByCurrentLambda.map(_ ++ names)" <>
          "val compiledBindings = bindings.view.map(_._2)" <>
            ".map(e => compile(builtins, e, boundByCurrentLambda2, recursiveVars, currentRec, IsNotTail))".indent <>
          "val compiledBody = compile(builtins, body, boundByCurrentLambda2, recursiveVars2, currentRec, isTail)" <>
          "// compile all the bindings and the body" <>
          "// to evaluate, evaluate all the bindings, getting back a `Rt` for each" <>
          "// then call bind on each" <>
          "val compiledBody2 = compiledBody // NB workaround for https://issues.scala-lang.org/browse/SI-10036" <>
          "val compiledBindings2 = compiledBindings" <>
          "val names2 = names" <>
          "trait B " + {
            "self: Rt => " <> {
              "def bind(env: Map[Name,Rt]) = " + {
                "// remove any bindings shadowed in local let rec" <>
                "val env2: Map[Name, Rt] = env -- names2" <>
                "if (env2.nonEmpty) " + {
                  "compiledBindings2.foreach(_.bind(env2))" <>
                  "compiledBody2.bind(env2)"
                }.b
              }.b
            }.indent
          }.b <>
          "// might want to 'de-rec' useless let recs since regular let code is going to be faster probably" <>
          "(arity(freeVars(e), env(e)) : @annotation.switch) match " + {
            (0 to N).each { i =>
              s"case $i => new Arity$i(e,()) with B " + {
                applySignature(i) + " = " + {
                  s"val evaluatedBindings = compiledBindings.map " + {
                    "rec => " <> {
                      s"val d = ${tailEval(i, "rec")}" <>
                      "r.toRuntime(d)"
                    }.indent
                  }.b <>
                  "val env: Map[Name, Rt] = names.zip(evaluatedBindings).toMap" <>
                  "evaluatedBindings.foreach(b => b.bind(env))" <>
                  "compiledBody.bind(env)" <>
                  tailEval(i, "compiledBody")
                }.b
              }.b
            } <>
            "case n => new ArityN(n,e,()) with B " + {
              "def apply(rec: Rt, args: Array[Slot], r: R) = " + {
                "val evaluatedBindings = compiledBindings.map " + {
                  "b => r.toRuntime(b(b, args, r))"
                }.b <>
                "val env = names.zip(evaluatedBindings).toMap" <>
                "evaluatedBindings.foreach(b => b.bind(env))" <>
                "compiledBody.bind(env)" <>
                "compiledBody(rec, args, r)"
              }.b
            }.b
          }.b
        }.b
    }.b
}