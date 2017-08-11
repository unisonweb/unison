package org.unisonweb.compilation

import org.unisonweb.Term.Name

object Let1 {
  def compileLet1(builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
    recursiveVars: Set[Name], currentRec: Option[(Name,Arity)], isTail: Boolean)(
    name: Name, binding: TermC, body: TermC): Rt = {
    val compiledBinding = compile(builtins, binding, boundByCurrentLambda, recursiveVars, currentRec, IsNotTail)
    val compiledBody =
      compile(builtins, body, boundByCurrentLambda.map(_ + name), recursiveVars - name,
        shadowRec(currentRec, name), isTail)
    val compiledBinding2 = compiledBinding
    val compiledBody2 = compiledBody
    trait LB { self: Rt =>
      def bind(env: Map[Name,Rt]) = {
        compiledBinding2.bind(env)
        compiledBody2.bind(env - name)
      }
    }
    (arity(freeVars(e), env(e)) : @annotation.switch) match {
      case 0 => new Arity0(e,()) with LB {
        def apply(rec: Rt, r: R) =
          compiledBody(rec, { try compiledBinding(rec, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
      }
      case 1 => new Arity1(e,()) with LB {
        def apply(rec: Rt, x1: D, x1b: Rt, r: R) =
          compiledBody(rec, { try compiledBinding(rec, x1, x1b, r) catch { case e: TC => loop(e,r) }}, r.boxed, x1, x1b, r)
      }
      case 2 => new Arity2(e,()) with LB {
        def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) =
          compiledBody(rec, { try compiledBinding(rec, x1, x1b, x2, x2b, r) catch { case e: TC => loop(e,r) }}, r.boxed, x1, x1b, x2, x2b, r)
      }
      case 3 => new Arity3(e,()) with LB {
        def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) =
          compiledBody(rec, { try compiledBinding(rec, x1, x1b, x2, x2b, x3, x3b, r) catch { case e: TC => loop(e,r) }}, r.boxed, x1, x1b, x2, x2b, x3, x3b, r)
      }
      case 4 => new Arity4(e,()) with LB {
        def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) =
          compiledBody(rec, Array(Slot({ try compiledBinding(rec, x1, x1b, x2, x2b, x3, x3b, x4, x4b, r) catch { case e: TC => loop(e,r) }}, r.boxed), Slot(x1, x1b), Slot(x2, x2b), Slot(x3, x3b), Slot(x4, x4b)), r)
      }
      case n => new ArityN(n,e,()) with LB {
        def apply(rec: Rt, args: Array[Slot], r: R) =
          compiledBody(rec,
            Slot(try compiledBinding(rec, args, r) catch { case e: TC => loop(e,r) }, r.boxed) +: args,
            r)
      }
    }
  }
}
