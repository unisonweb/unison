package org.unisonweb
package compilation

import org.unisonweb.Term.{Name, Var}

object FunctionApplication {
  def compileFunctionApplication(
    builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
    recursiveVars: Set[Name], currentRec: Option[(Name,Arity)], isTail0: Boolean)(
    fn: TermC, args: List[TermC]): Rt = {

    // don't bother with tail calls for builtins and nonrecursive calls
    val isTail = isTail0 //todo think through conditions to safely elide tailcall
    // && { fn match { case Var(v) if recursiveVars.contains(v) => true; case _ => false }}

    (fn, args) match {
      // Term can represent this call with no arguments, though the parser would never produce it.
      case (fn, List()) =>
        compile(builtins, fn, boundByCurrentLambda, recursiveVars, currentRec, isTail)

      // fully saturated call to the nearest enclosing recursive function
        // e.g. fac n = n * (fac (n-1))
        //                  ^^^^^^^^^^^
      case (Var(v), args) if Some((v,args.length)) == currentRec =>
        val compiledArgs = args.view.map(compile(builtins, _, boundByCurrentLambda, recursiveVars, currentRec, IsNotTail)).toArray
        compilation.StaticCall.staticRecCall(compiledArgs, unTermC(e), isTail)

      case _ =>
        /* Two cases to consider:
          1. static call, eg `id 42`
          2. dynamic call, eg `(f x -> f x) id 42`
                                       ^^^ is a dynamic application
         */
        val compiledFn = compile(builtins, fn, boundByCurrentLambda, recursiveVars, currentRec, IsNotTail)
        val compiledArgs = args.view.map(arg =>
          compile(builtins, arg, boundByCurrentLambda, recursiveVars, currentRec, IsNotTail)
        ).toArray

        if (compiledFn.isEvaluated)
          compilation.StaticCall.staticCall(compiledFn, compiledArgs, unTermC(e), isTail)
        else DynamicCall.dynamicCall(compiledFn, compiledArgs, unTermC(e), isTail)
    }
  }
}
