package org.unisonweb

import org.unisonweb.Term.{Name, Term}

package compilation {
  case class Slot(var unboxed: D, var boxed: Value)
  case class Result(var boxed: Value)

  case class CurrentRec(get: Option[(Name, Arity)]) extends AnyVal {
    def contains(name: Name): Boolean = get.exists(_._1 == name)
    def contains(name: Name, arity: Arity): Boolean = get == Some((name, arity))
    /** knock out the currentRec if appropriate; if it is shadowed, it won't be called */
    def shadow(name: Name): CurrentRec = CurrentRec(get.filterNot(name == _._1))
    /** knock out the currentRec if appropriate; if it is shadowed, it won't be called */
    def shadow(names: Seq[Name]): CurrentRec = CurrentRec(get.filterNot(names contains _._1))
  }
  object CurrentRec {
    def empty = CurrentRec(None)
    def apply(name: Name, arity: Arity): CurrentRec = CurrentRec(Some((name,arity)))
  }

}

package object compilation extends TailCalls with CompileLet1 with CompileLetRec with LookupVar with FunctionApplication with CompileIf0 {
  type D = Double
  type V = Value
  type R = Result
  type TC = TailCall
  type Arity = Int
  val IsTail = true
  val IsNotTail = false

  /**
   * The annotation contains a `Set[Name]` of free variables for the term,
   * and a `Vector[Name]` which is a stack of bound variables at the term
   * (bound variable stack is also called "the environment").
   *
   * Ex: `x -> y -> x + y`, free variables of `x + y` will be `Set(x, y)`,
   * and bound variables will be `Vector(y, x)`.
   */
  type TermC = ABT.AnnotatedTerm[Term.F, (Set[Name], Vector[Name])]

  def unTermC(t: TermC): Term = t.map(_._1)

  def env(t: TermC): Vector[Name] = t.annotation._2

  def freeVars(t: TermC): Set[Name] = t.annotation._1

  def stackSize(t: TermC): Int = stackSize(freeVars(t), env(t))


  /**
   * Given a set of free variables, and a stack of bound variables, figure out
   * how many elements from `bound` stack we need to be able to resolve all free vars.
   *
   * Ex: Set(x,y) and bound = Vector(x,p,q,r,y,z), arity would be: 5, since we need `bound.take(5)`
   * to have access to both `x` and `y`.
   */
  def stackSize(freeVars: Set[Name], bound: Vector[Name]): Int =
    if (freeVars.isEmpty) 0
    else freeVars.view.map(fv => bound.indexOf(fv)).max + 1

  def compile(builtins: String => Value)(e: Term): Computation = {
    compile(builtins, ABT.annotateBound(e), CurrentRec(None), IsTail)
    // todo: do something with builtins
  }
  def compile(builtins: String => Value, e: TermC, currentRec: CurrentRec, isTail: Boolean): Computation = {
    @inline def compile1(isTail: Boolean)(e: TermC): Computation = compile(builtins, e, currentRec, isTail)
    e match {
      case Term.Num(n) => compileNum(n)
      case Term.Builtin(name) => ???
      case Term.Compiled(c) => ??? // c
      case Term.Var(name) =>
        if (currentRec.contains(name))
          compileRecVar(name)
        else
          env(e).indexOf(name) match {
            case -1 => sys.error("unknown variable: " + name)
            case i => lookupVar(i, unTermC(e))
          }
      case Term.If0(cond, if0, ifNot0) =>
        val compiledCond = compile1(IsNotTail)(cond)
        val compiledIf0 = compile1(isTail)(if0)
        val compiledIfNot0 = compile1(isTail)(ifNot0)

        compileIf0(compiledCond, compiledIf0, compiledIfNot0, unTermC(e))

      case Term.Lam(names, body) =>
        // codegen Lambda1, Lambda2, ... Lambda<max-arity> will extend Lambda
        // they should handle over and under-application in the different apply methods
        // only reason we needed to pass more than just the compiled body + metadata (names, orig term)
        // is to handle under-application, because under-application would substitute away the
        // bound variables, and then recompile
        // question - can we just
        ??? // compileLambda(e, boundByCurrentLambda, currentRec)(names, body)
        // old code starts out like this:
        // if (freeVars(e).isEmpty) makeLambda
        // else {
        //   val locallyBound = freeVars(body).filter(v => !recursiveVars.contains(v))
        //   ... stuff ...
        // }

        // Lamdba3 (not Lambda_3) ctor receives (argName1: Name, argName2: Name, argName3: Name, e: => Term, body: => Term, compiledBody: Rt, builtins: String => Rt)
      case Term.LetRec(bindings, body) =>
        val compiledBindings = bindings.view.map(_._2).map(compile1(IsNotTail)).toArray
        val compiledBody = compile1(isTail)(body)

        compileLetRec(e, compiledBindings, compiledBody)

      case Term.Let1(name, binding, body) =>
        val compiledBinding = compile1(IsNotTail)(binding)
        val compiledBody = compile1(isTail)(body)

        compileLet1(compiledBinding, compiledBody, unTermC(e))

      case Term.Apply(fn, args) =>
        //todo think through conditions to safely elide tailcall
        (fn, args) match {
          // Term can represent this call with no arguments, though the parser would never produce it.
          case (fn, List()) =>
            sys.error("the parser isn't supposed to produce this")

          // fully saturated call to the nearest enclosing recursive function
          // e.g. fac n = n * (fac (n-1))
          //                  ^^^^^^^^^^^
          case (Term.Var(v), args) if currentRec.contains(v, args.length) =>
            val compiledArgs = args.view.map(compile1(IsNotTail)).toArray
            compilation.staticRecCall(compiledArgs, unTermC(e), isTail)

          // todo: what if it's an overapplication / underapplication of nearest enclosing recursive function; becomes dynamic application?

          case _ =>
            /* Two cases to consider:
              1. static call, eg `id 42`
              2. dynamic call, eg `(f x -> f x) id 42`
                                           ^^^ is a dynamic application
             */
            val compiledArgs = args.view.map(compile1(IsNotTail)).toArray
            compile1(IsNotTail)(fn) match {
              // if cast fails, then it's a type error: applying arguments to non-Lambda value
              case Return(fn) => staticCall(fn.asInstanceOf[Lambda], compiledArgs, unTermC(e), isTail)
              case compiledDynamic => dynamicCall(compiledDynamic, compiledArgs, unTermC(e), isTail)
            }
        }
    }
  }

  def compileNum(d: Double) = {
    class CompiledNum extends Computation0(Term.Num(d)) {
      def apply(rec: Lambda, r: R) = d
    }
    new CompiledNum
  }

  def compileRecVar(name: Name) = {
    class RecursiveReference extends Computation0(Term.Var(name)) {
      def apply(rec: Lambda, r: R) = { r.boxed = rec; 0.0 }
    }
    new RecursiveReference
  }


}
