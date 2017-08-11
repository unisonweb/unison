package org.unisonweb.codegeneration

object CompileLet1Generator extends OneFileGenerator("CompileLet1.scala") {
  def source: String =
    s"""package org.unisonweb.compilation
       |
       |import org.unisonweb.Term.Name
       |
       |trait CompileLet1 {
       |  def compileLet1(builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
       |    recursiveVars: Set[Name], currentRec: Option[(Name,Arity)], isTail: Boolean)(
       |    name: Name, binding: TermC, body: TermC): Rt = {
       |    val compiledBinding = compile(builtins, binding, boundByCurrentLambda, recursiveVars, currentRec, IsNotTail)
       |    val compiledBody =
       |      compile(builtins, body, boundByCurrentLambda.map(_ + name), recursiveVars - name,
       |        shadowRec(currentRec, name), isTail)
       |    val compiledBinding2 = compiledBinding
       |    val compiledBody2 = compiledBody
       |    trait LB { self: Rt =>
       |      def bind(env: Map[Name,Rt]) = {
       |        compiledBinding2.bind(env)
       |        compiledBody2.bind(env - name)
       |      }
       |    }
       |    (arity(freeVars(e), env(e)) : @annotation.switch) match {""".stripMargin <>
            (0 to N).each { i =>
              s"case $i => new Arity$i(e,()) with LB " + {
                applySignature(i) + " =" <> {
                  if (i < N) "compiledBody(rec, " + eval(i, "compiledBinding") + ", r.boxed, " + xArgs(i) + commaIf(i) + "r)"
                  else s"compiledBody(rec, Array(Slot(${eval(N, "compiledBinding")}, r.boxed), " + (0 until N).commas(slot) + "), r)"
                }.indent
              }.b
            }.indentBy(3) <>
     """      case n => new ArityN(n,e,()) with LB {
       |        def apply(rec: Rt, args: Array[Slot], r: R) =
       |          compiledBody(rec,
       |            Slot(try compiledBinding(rec, args, r) catch { case e: TC => loop(e,r) }, r.boxed) +: args,
       |            r)
       |      }
       |    }
       |  }
       |}
    """.stripMargin
}