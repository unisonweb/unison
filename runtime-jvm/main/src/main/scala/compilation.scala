package org.unisonweb

import org.unisonweb.Term.{Name, Term}

package compilation {
  case class Slot(var unboxed: D, var boxed: Value)
  case class Result(var boxed: Value = null)

  case class CurrentRec(get: Option[(Name, Arity)]) extends AnyVal {
    def contains(name: Name): Boolean = get.exists(_._1 == name)
    def contains(name: Name, arity: Arity): Boolean = get == Some((name, arity))
    /** knock out the currentRec if appropriate; if it is shadowed, it won't be called */
    def shadow(name: Name): CurrentRec = CurrentRec(get.filterNot(name == _._1))
    /** knock out the currentRec if appropriate; if it is shadowed, it won't be called */
    def shadow(names: Seq[Name]): CurrentRec = CurrentRec(get.filterNot(names contains _._1))
  }
  object CurrentRec {
    def none = CurrentRec(None)
    def apply(name: Name, arity: Arity): CurrentRec = CurrentRec(Some((name,arity)))
  }

}

package object compilation extends TailCalls with CompileLambda with CompileLet1 with CompileLetRec with LookupVar with CompileFunctionApplication with CompileIf0 {
  type D = Double
  type V = Value
  type R = Result
  type TC = TailCall
  type Arity = Int
  type IsTail = Boolean
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

  def warnAssert(b: Boolean, s: => String) = if (!b) {
    System.err.println(s)
    Thread.dumpStack()
  }


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

  def compile(builtins: String => Computation)(e: Term): Computation =
    compile(builtins, checkedAnnotateBound(e), CurrentRec.none, IsTail)

  def checkedAnnotateBound(e: Term) = {
    assert(e.annotation.isEmpty, s"reannotating term with free vars: ${e.annotation}\n" + Render.renderIndent(e))
    ABT.annotateBound(e)
  }

  def compile(builtins: String => Computation, termC: TermC, currentRec: CurrentRec, isTail: IsTail): Computation = {
    // System.out.println("[debug] compiling:\n" + Render.renderIndent(termC))

    @inline def compile1(isTail: IsTail)(termC: TermC): Computation = compile(builtins, termC, currentRec, isTail)
    @inline def compile2(isTail: IsTail, currentRec: CurrentRec)(termC: TermC): Computation = compile(builtins, termC, currentRec, isTail)

    termC match {
      case Term.Num(n) => compileNum(n)
      case Term.Builtin(name) => builtins(name)
      case Term.Compiled(c) => Return(c)(unTermC(termC)) // todo: can we do a better job tracking the uncompiled form?
      case Term.Var(name) => compileVar(currentRec, name, termC)
      case Term.If0(cond, if0, ifNot0) =>
        val compiledCond = compile1(IsNotTail)(cond)
        val compiledIf0 = compile1(isTail)(if0)
        val compiledIfNot0 = compile1(isTail)(ifNot0)

        compileIf0(termC, compiledCond, compiledIf0, compiledIfNot0)

      case Term.Lam(names, body) =>
        // codegen Lambda1, Lambda2, ... Lambda<max-arity> will extend Lambda
        // they should handle over and under-application in the different apply methods
        // only reason we needed to pass more than just the compiled body + metadata (names, orig term)
        // is to handle under-application, because under-application would substitute away the
        // bound variables, and then recompile

        // grab all the free variables referenced by the body of the lambda (not bound by the lambda itself)
        // get them off the stack when you build the lambda
        //  (compile/get all those `Var`s)
        //  take those Values,
            // decompile them,
            // substitute them into the body of the lambda, being careful about name clashes
            // now the lambda has no more free variables; good to compile with happy path

        compileLambda(termC, names, body, currentRec, r => t => compile2(IsTail, r)(checkedAnnotateBound(t)))

      case Term.LetRec(bindings, body) =>
        val shadowCurrentRec = currentRec.shadow(bindings.map(_._1))
        val compiledBindings: Array[Computation] = bindings.view.map {
          case (name, l@Term.Lam(args, body)) => // possibly self-recursive
            compile2(IsNotTail, CurrentRec(name, args.length))(l)
          case (_, bindingTermC) => // not a lambda, shouldn't contain recursive call
            compile2(IsNotTail, shadowCurrentRec)(bindingTermC) // todo should these both replace currentRec?
        }.toArray
        val compiledBody = compile2(isTail, shadowCurrentRec)(body)

        compileLetRec(termC, compiledBindings, compiledBody)

      case Term.Let1(name, binding, body) =>
        val compiledBinding = compile2(IsNotTail, currentRec)(binding)
        val compiledBody = compile2(isTail, currentRec.shadow(name))(body)

        compileLet1(termC, compiledBinding, compiledBody)

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
            compilation.staticRecCall(termC, compiledArgs, isTail)


          case _ =>
            /* Two cases to consider:
              1. static call, eg `id 42`
              2. dynamic call, eg `(f x -> f x) id 42`
                                           ^^^ is a dynamic application
              todo: may want to break out a self-call that isn't fully saturated; special case of dynamicCall
                    could conceivably be done more efficiently
             */
            val compiledArgs = args.view.map(compile1(IsNotTail)).toArray
            compile1(IsNotTail)(fn) match {
              // if cast fails, then it's a type error: applying arguments to non-Lambda value
              case Return(fn: Lambda) => staticCall(termC, fn, compiledArgs, isTail)
              case compiledDynamic => dynamicCall(termC, compiledDynamic, compiledArgs, isTail)
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

  def compileVar(currentRec: CurrentRec, name: Name, termC: TermC): Computation = {
    // System.out.println("[debug] compileVar: " + Render.render(termC))

    if (currentRec.contains(name))
      compileRecVar(name)
    else
      env(termC).indexOf(name) match {
        case -1 => sys.error("unknown variable: " + name + "\nin " + Render.renderIndent(termC))
        case i => lookupVar(i, unTermC(termC))
      }
  }

  def compileRecVar(name: Name) = {
    class RecursiveReference extends Computation0(Term.Var(name)) {
      def apply(rec: Lambda, r: R) = { r.boxed = rec; 0.0 }
    }
    new RecursiveReference
  }
}
