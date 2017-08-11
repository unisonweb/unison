package org.unisonweb
package compilation

import org.unisonweb.Term.Name

object LetRec1 {
  def compileLetRec1(
    builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
    recursiveVars: Set[Name], currentRec: Option[(Name,Arity)], isTail: Boolean)(
    name: Name, vs: List[Name], f: TermC, bodyf: TermC, body: TermC): Rt =
    if (hasTailRecursiveCall(name, vs.length, bodyf)) {
      val vsv = vs.toVector
      val compiledf = {
        val step = compilation.Lambda.compileLambda(builtins, f, boundByCurrentLambda, recursiveVars + name,
          Some((name,vsv.length)))(vs, bodyf)
        @annotation.tailrec
        def loop(v1: D, v1b: Rt, r: R): Double =
          try return step(step, v1, v1b, r)
          catch { case e: SelfCall => loop(e.x1, e.x1b, r) }
        new Lambda1(vsv(0), unTermC(f), step) {
          override def apply(rec: Rt, x1: D, x1b: Rt, r: R) = loop(x1, x1b, r)
        }
      }
      // we compile this just like a let1
      val compiledBody = compile(builtins, body, boundByCurrentLambda, recursiveVars - name, currentRec, isTail)
      (arity(freeVars(e), env(e)) : @annotation.switch) match {
        case 0 => new Arity0(e,()) {
          def bind(env: Map[Name,Rt]) = compiledf bind (env -- vs - name)
          def apply(rec: Rt, r: R) =
          // NB: okay to assume compiledf is already evaluated, since it is a lambda with no free vars
            compiledBody(rec, 0.0, compiledf, r)
        }
        case 1 => new Arity1(e,()) {
          def bind(env: Map[Name,Rt]) = compiledf bind (env -- vs - name)
          def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
            val compiledf2 =
              if (compiledf.isEvaluated) compiledf
              else { { try compiledf(rec, x1, x1b, r) catch { case e: TC => loop(e,r) }}; r.boxed }
            compiledBody(rec, 0.0, compiledf2, x1, x1b, r)
          }
        }
        case 2 => new Arity2(e,()) {
          def bind(env: Map[Name,Rt]) = compiledf bind (env -- vs - name)
          def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
            val compiledf2 =
              if (compiledf.isEvaluated) compiledf
              else { { try compiledf(rec, x1, x1b, x2, x2b, r) catch { case e: TC => loop(e,r) }}; r.boxed }
            compiledBody(rec, 0.0, compiledf2, x1, x1b, x2, x2b, r)
          }
        }
      }
    }
    else ???
}