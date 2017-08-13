package org.unisonweb.codegeneration

object LambdaGenerator extends OneFileGenerator("Lambda.scala") {
  def lambdaCtorArgs(i: Int): String =
    (1 to i).commas(i => s"name$i: Name")

  def bindFixed(i: Int): String =
    "var bound: Map[Name, Rt] = Map.empty" <>
    "" <>
    "override def bind(env: Map[Name, Rt]) = " + {
      "val env2 = env" + (1 to i).map(" - name" + _).mkString <>
        "compiledBody.bind(env2)" <>
        "bound = bound ++ env2"
    }.b <> ""

  def fixedApplyDefs(i: Int) =
    0 to N each {
      case 0 =>
        "// apply 0 arguments / no-op" <>
        s"override ${applySignature(0)} = { r.boxed = this; 0.0 }" <> ""

      case j if j < i =>
        val unboundNames = (j until i).commas(k => s"name${k + 1}")
        s"// partial application: $j of $i arguments" <>
        s"override ${applySignature(j)} = " + {
          "ABT.substs(Map(" +
            (0 until j).commas { k => s"\n(name${k + 1}, Compiled(toRuntime(x${j - 1 - k}, x${j - 1 - k}b)))" }.indent <>
            ")" + (j+1 to i).map(" - name" + _).mkString + ")(body) match " + {
              s"case body => " <> {
                s"val tm = Lam($unboundNames)(body)" <>
                  (i - j match {
                    case 1 => s"val lam = new Lambda1($unboundNames, tm, compile(builtins)(body))"
                    case k => s"val lam = new Lambda$k($unboundNames, tm, body, compile(builtins)(body), builtins)"
                  }) <>
                  "lam.bind(bound)" <>
                  "r.boxed = lam" <>
                  "0.0"
              }.indent
            }.b
        }.b <> ""

      case j if j == i =>
        s"// exact application: all $i arguments" <>
        s"override ${applySignature(i)} =" <>
        tailEval(i, "compiledBody").indent <> ""

      case j /* j > i */ =>
        s"// over-application: all $i arguments + ${j-i} more" <>
        s"override ${applySignature(j)} = " + {
          tailEval(i, "apply") <>
            "r.boxed.apply(r.boxed, " + xArgs(i, j-i) + commaIf(j-i) + "r)"
        }.b <> ""
    }

  def fixedOverapplyN(expected: Int): String =
    s"// over-application with ${N+1} or more args: all $expected arguments + at least ${N+1-expected} more" <>
    "override def apply(rec: Rt, xs: Array[Slot], r: R): D = " + {
      s"apply(rec, ${xsArgs(expected)}, r)" <>
        s"xs.drop($expected) match " + {
          "case xs => (xs.length : @annotation.switch) match " + {
            ((N+1-expected) to N).each { i => s"case $i => r.boxed.apply(r.boxed, ${xsArgs(i)}, r)" } <>
              s"case n if n > $N => r.boxed.apply(r.boxed, xs, r)"
          }.b
        }.b
    }.b <> ""

  def source =
    "package org.unisonweb.compilation" <>
    "" <>
    "import org.unisonweb.ABT" <>
    "import org.unisonweb.Term.{Compiled, Lam, Name, Term}" <>
    "" <>
    s"/** Specialized Lambda of 1 parameter */" <>
    "class Lambda1(name: Name, e: => Term, compiledBody: Rt) extends Arity1(e) " + {
      "override def bind(env: Map[Name,Rt]): Unit = compiledBody.bind(env - name)" <>
      "" <>
      fixedApplyDefs(1) <>
      fixedOverapplyN(1) <>
      "override def isEvaluated = true"
    }.b <>
    "" <>
    (2 to N).each { i => // x -> (x -> 1)  // ((x x -> 1)  5(x2) 6(x1))
      s"/** Specialized Lambda of $i parameter */" <>
      s"class Lambda$i(${lambdaCtorArgs(i)}, e: => Term, body: => Term, compiledBody: Rt, builtins: String => Rt) extends Arity$i(e) " + {
        bindFixed(i) <>
        fixedApplyDefs(i) <>
        fixedOverapplyN(i) <>
        "override def isEvaluated = true"
      }.b <> ""
    } <>
    s"/** Lambda with ${N+1} or more parameters */" <>
    "class LambdaN(names: Array[Name], e: => Term, body: => Term, compiledBody: Rt, builtins: String => Rt) extends ArityN(names.length, e)" + {
      """var bound: Map[Name,Rt] = Map.empty
      |override def bind(env: Map[Name,Rt]) = if (env.isEmpty) () else {
      |  val env2 = env -- names
      |  compiledBody.bind(env2)
      |  bound = bound ++ env2
      |}
    """.stripMargin <>
    (0 to N).each {
      case 0 =>
        "// apply 0 arguments / no-op" <>
        s"override ${applySignature(0)} = { r.boxed = this; 0.0 }" <> ""

      case j =>
        s"// partial application: $j of ${N+1}+ arguments; ${N+1-j}+ additional arguments needed" <>
        s"override ${applySignature(j)} = " + {
          s"val unboundNames = names.drop($j)" <>
          s"assert(unboundNames.length >= ${N+1-j})" <>
          "val lam = ABT.substs(Map(" +
            (0 until j).commas { k => s"\n(names($k), Compiled(toRuntime(x${j - 1 - k}, x${j - 1 - k}b)))" }.indent <>
          s") -- unboundNames)(body) match " + {
              "case body => " <> {
                "val tm = Lam(unboundNames: _*)(body)" <>
                "unboundNames match " + {
                  (N - j + 1 to N).each {
                    case 1 =>
                      s"case Array(name) =>" <>
                        "new Lambda1(name, tm, compile(builtins)(body))".indent
                    case k =>
                      val names = (j + 1) until (j + 1 + k) commas (i => s"name$i")
                      s"case Array($names) =>" <>
                        s"new Lambda$k($names, tm, body, compile(builtins)(body), builtins)".indent
                  } <>
                  "case unboundNames =>" <>
                    "new LambdaN(unboundNames, tm, body, compile(builtins)(body), builtins)".indent
                }.b
              }.indent
            }.b <>
            "lam.bind(bound)" <>
            "r.boxed = lam" <>
            "0.0"
        }.b <>
        ""
    } <>
    s"override def apply(rec: Rt, xs: Array[Slot], r: R): D = " + {
      "if (xs.length == names.length) compiledBody(rec, xs, r)" <>
      "else if (xs.length < names.length) " + {
        "// under-application" <>
          "val unboundNames = names.drop(xs.length)" <>
          "val lam = ABT.substs((0 until xs.length).map " + {
            "i => val x = xs(xs.length - 1 - i); (names(i), Compiled(toRuntime(x.unboxed, x.boxed)))"
          }.b + ".toMap -- unboundNames)(body) match " + {
            "case body =>" <> {
              "val tm = Lam(unboundNames: _*)(body)" <>
              "unboundNames match " + {
                "case Array(name) =>" <>
                  "new Lambda1(name, tm, compile(builtins)(body))".indent <>
                (2 to N).each { i =>
                  val names = (1 to i).commas(j => "name" + j).mkString
                  s"case Array($names) =>"<>
                    s"new Lambda$i($names, tm, body, compile(builtins)(body), builtins)".indent
                } <>
                "case names => new LambdaN(names, tm, body, compile(builtins)(body), builtins)"
              }.b
            }.indent
          }.b <>
          "lam.bind(bound)" <>
          "r.boxed = lam" <>
          "0.0"
      }.b <>
      "else " + {
        "// over-application" <>
        "apply(rec, xs.take(names.length), r)" <>
        "xs.drop(names.length) match " + {
          "case xs => (xs.length : @annotation.switch) match " + {
            (1 to N).each { i =>
              s"case $i => r.boxed.apply(r.boxed, ${xsArgs(i)}, r)"
            } <>
            s"case n if n > $N => r.boxed.apply(r.boxed, xs, r)"
          }.b
        }.b
      }.b
    }.b
  }.b
}
