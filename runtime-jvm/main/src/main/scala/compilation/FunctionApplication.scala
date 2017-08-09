package org.unisonweb
package compilation

import org.unisonweb.Term.{Compiled, Name, Var}

object FunctionApplication {
  def compileFunctionApplication(
    builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
    recursiveVars: Set[Name], currentRec: Option[(Name,Arity)], isTail0: Boolean)(
    fn: TermC, args: List[TermC]): Rt = {
    // don't bother with tail calls for builtins and nonrecursive calls
    val isTail = isTail0 && { fn match { case Var(v) if recursiveVars.contains(v) => true; case _ => false }}
    (fn, args) match {
      case (fn, List()) =>
        compile(builtins, fn, boundByCurrentLambda, recursiveVars, currentRec, isTail)
      case (Var(v), args) if Some((v,args.length)) == currentRec =>
        val compiledArgs = args.view.map(compile(builtins, _, boundByCurrentLambda, recursiveVars, currentRec, IsNotTail)).toArray
        compilation.StaticCall.staticRecCall(compiledArgs, unTermC(e), isTail)
      case _ =>
        /* Four cases to consider:
           1. static (fn already evaluated, known arity), fully-saturated call (correct # args),
              ex `(x -> x) 42`
           2. static partial application, ex `(x y -> x) 42`, need to form closure or specialize
           3. static overapplication, ex `(x -> x) (y -> y) 42` or `id id 42`
           4. dynamic application, ex in `(f x -> f x) id 42`, `f x` is a dynamic application
        */
        val compiledFn = compile(builtins, fn, boundByCurrentLambda, recursiveVars, currentRec, IsNotTail)
        val compiledArgs = args.view.map(arg =>
          compile(builtins, arg, boundByCurrentLambda, recursiveVars, currentRec, IsNotTail)
        ).toArray
        // NB workaround for https://issues.scala-lang.org/browse/SI-10036
        val compiledFn2 = compiledFn
        val compiledArgs2 = compiledArgs
        trait FAB { self: Rt =>
          def bind(env: Map[Name,Rt]) = if (env.isEmpty) () else {
            compiledFn2.bind(env)
            compiledArgs2.foreach(_.bind(env))
          }
        }
        if (compiledFn.isEvaluated) {
          if (compiledFn.arity >= compiledArgs.length) // 1. and 2. - exact / under-application
            compilation.StaticCall.staticCall(compiledFn, compiledArgs, unTermC(e), isTail)
          else { // 3. overapplication (compiledFn.arity < compiledArgs.length)
            val (passed, extra) = compiledArgs.splitAt(compiledFn.arity)
            val fn2 = compilation.StaticCall.staticCall(compiledFn, passed, unTermC(e), isTail)
            compile(builtins,
              ABT.annotateBound(Compiled(fn2)(extra.map(Compiled(_)): _*)),
              boundByCurrentLambda, recursiveVars, currentRec, isTail)
          }
        }
        else // 4.
        // TODO - factor this out into DynamicCallGenerator
          arity(freeVars(e), env(e)) match {
            case 0 => compiledArgs.length match {
              case 1 => new Arity0(e,()) with FAB {
                val arg = compiledArgs(0)
                def apply(rec: Rt, r: R) =
                  if (compiledFn.isEvaluated)
                    compiledFn(compiledFn, { try arg(rec, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
                  else {
                    { try compiledFn(rec, r) catch { case e: TC => loop(e,r) }}
                    val fn = r.boxed
                    if (fn.arity == 1) fn(fn, { try arg(rec, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
                    else if (fn.arity > 1)
                      sys.error("todo - handle partial application here")
                    else sys.error("type error, function of arity: " + fn.arity + " applied to 1 argument")
                  }
              }
            }
            case 1 => compiledArgs.length match {
              case 1 => new Arity1(e,()) with FAB {
                val arg = compiledArgs(0)
                def apply(rec: Rt, x1: D, x1b: Rt, r: R) =
                  if (compiledFn.isEvaluated)
                    compiledFn(compiledFn, { try arg(rec, x1, x1b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
                  else {
                    { try compiledFn(rec, x1, x1b, r) catch { case e: TC => loop(e,r) }}
                    val fn = r.boxed
                    if (fn.arity == 1) fn(fn, { try arg(rec, x1, x1b, r) catch { case e: TC => loop(e,r) }}, r.boxed, r)
                    else if (fn.arity > 1)
                      sys.error("todo - handle partial application here")
                    else sys.error("type error, function of arity: " + fn.arity + " applied to 1 argument")
                  }
              }
            }
          }
    }
  }
}
