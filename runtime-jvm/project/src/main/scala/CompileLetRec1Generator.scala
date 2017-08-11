package org.unisonweb.codegeneration

object CompileLetRec1Generator extends OneFileGenerator("CompileLetRec1.scala") {
  def source: String =
    s"""package org.unisonweb.compilation
       |
       |import org.unisonweb.Term.Name
       |
       |trait CompileLetRec1 {
       |  // LetRec1 is a special case of LetRec which only has one binding
       |  // e.g.  let rec fac n acc = if n > 0 then fac (n-1) (n * acc) else 1; fac 3 1
       |      // e: let rec fac n acc = if n > 0 then fac (n-1) (n * acc) else 1; fac 3 1
       |      // name: fac
       |      // vs: List(n, acc)
       |      // f:  n acc -> if n > 0 then fac (n-1) (n * acc) else 1; fac 3 1
       |      // bodyf: if n > 0 then fac (n-1) (n * acc) else 1; fac 3 1
       |      // body: fac 3 1
       |  def compileLetRec1(
       |    builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
       |    recursiveVars: Set[Name], currentRec: Option[(Name,Arity)], isTail: Boolean)(
       |    name: Name, vs: List[Name], f: TermC, bodyf: TermC, body: TermC): Rt = {
       |
       |    val vsv = vs.toVector
       |    val compiledf = {
       |      val step = compileLambda(builtins, f, boundByCurrentLambda, recursiveVars + name, Some((name,vsv.length)))(vs, bodyf)
       |      if (hasTailRecursiveCall(name, vs.length, bodyf)) {
       |        @annotation.tailrec
       |        def loop(v1: D, v1b: Rt, r: R): Double =
       |          try step(step, v1, v1b, r)
       |          catch { case e: SelfCall => loop(e.x1, e.x1b, r) }
       |        new Lambda1(vsv(0), unTermC(f), step) {
       |          override def apply(rec: Rt, x1: D, x1b: Rt, r: R) = loop(x1, x1b, r)
       |        }
       |      }
       |      else step
       |    }
       |
       |    // we compile this just like a let1
       |    val compiledBody = compile(builtins, body, boundByCurrentLambda, recursiveVars + name, currentRec, isTail)
       |    (arity(freeVars(e), env(e)) : @annotation.switch) match {""".stripMargin <>
          {
            (0 to N).each { i =>
              s"case $i => new Arity$i(e,()) " + {
                "def bind(env: Map[Name,Rt]) = { compiledf bind (env - name); compiledBody bind (env - name) }" <>
                applySignature(i) + " = " + {
                  "val compiledf2 = " <> {
                    "if (compiledf.isEvaluated) compiledf" <>
                    s"else { ${eval(i, "compiledf")}; r.boxed }"
                  }.indent <> {
                    if (i < N)
                      "compiledBody(rec, 0.0, compiledf2, " + xArgs(i) + commaIf(i) + "r)"
                    else
                      s"compiledBody(rec, Array(Slot(0.0, compiledf2), ${0 until N commas slot}), r)"
                  }
                }.b
              }.b
            }.indent <>
            "case n => new ArityN(n,e,()) " + {
              "def bind(env: Map[Name,Rt]) = { compiledf bind (env - name); compiledBody bind (env - name) }" <>
                "def apply(rec: Rt, xs: Array[Slot], r: R): D =" + {
                  "val compiledf2 = " <> {
                    "if (compiledf.isEvaluated) compiledf" <>
                      s"else { compiledf(rec, xs, r); r.boxed }"
                  }.indent <>
                    "compiledBody(rec, Slot(0.0, compiledf2) +: xs, r)"
                }.b
            }.b
          }.indentBy(3) <>
     """    } // match
       |  } // def compileLetRec1
       |} // object LetRec1
     """.stripMargin
}
