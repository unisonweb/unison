package org.unisonweb

/** short top-level classes */
package compilation {

  import org.unisonweb.Term.Name

  class TailCall(val fn: Rt, val x1: D, val x1b: Rt, val x2: D, val x2b: Rt, val args: Array[Slot]) extends Throwable {
    override def fillInStackTrace = this
  }

  class SelfCall(x1: D, x1b: Rt, x2: D, x2b: Rt, args: Array[Slot]) extends TailCall(null,x1,x1b,x2,x2b,args)

  case class Result(var boxed: Rt = null) {
    def toRuntime(unboxed: D): Rt = if (boxed eq null) compileNum(unboxed) else boxed
  }

  /** Used for representing parameters passed to `Runtime.apply` for large number of parameters. */
  case class Slot(var unboxed: D = 0,
    var boxed: Rt = null)

  // todo: exception for doing algebraic effects
  case class Yielded(effect: Rt, continuation: Rt) extends Throwable

  trait NF { self: Rt =>
    override def isEvaluated = true
    def bind(env: Map[Name,Rt]) = ()
  }

  trait AccumulateBound { self : Rt =>
    var bound : Map[Name,Rt] = Map.empty
    def bind(env: Map[Name,Rt]) = bound = env ++ bound
  }
}

/** top-level values */
package object compilation extends LookupVar with CompileIf0 with CompileLet1 with CompileVar
  with CompileLambda with CompileLetRec with CompileLetRec1 with TailCalls {

  import Term.{freeVars => _, _}

  type D = Double
  type Rt = Runtime
  type R = Result
  type TC = TailCall

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

  /**
   * Given a set of free variables, and a stack of bound variables, figure out
   * how many elements from `bound` stack we need to be able to resolve all free vars.
   *
   * Ex: Set(x,y) and bound = Vector(x,p,q,r,y,z), arity would be: 5, since we need `bound.take(5)`
   * to have access to both `x` and `y`.
   */
  def arity(freeVars: Set[Name], bound: Vector[Name]): Int =
    if (freeVars.isEmpty) 0
    else freeVars.view.map(fv => bound.indexOf(fv)).max + 1

  /** Constant indicating current term is in tail position, should be compiled accordingly. */
  val IsTail = true // switch this to false to disable all tail calls (may cause fireworks)

  /** Constant indicating current term not in tail position, should be compiled accordingly. */
  val IsNotTail = false

  /**
   * This is the main public compilation function. Takes a function for resolving builtins, a term,
   * and returns a `Runtime`.
   */
  def compile(builtins: String => Rt)(e: Term): Rt =
    compile(builtins, ABT.annotateBound(e), None, Set(), None, IsTail)

  /** Compile and evaluate a term, the return result back as a term. */
  def normalize(builtins: String => Rt)(e: Term): Term = {
    val rt = compile(builtins)(e)
    val r = Result()
    decompileSlot(try rt(null, r) catch { case e: TC => loop(e,r) }, r.boxed)
  }

  private def unbindRecursiveVars(e: TermC, recursiveVars: Set[Name]): TermC =
    e.reannotate { case (free,bound) => (free, bound.filterNot(recursiveVars.contains)) }

  type Arity = Int
  /** knock out the currentRec if appropriate; if it is shadowed, it won't be called */
  def shadowRec(rec: Option[(Name,Arity)], name: Name): Option[(Name,Arity)] =
    rec.filterNot(name == _._1)

  /** knock out the currentRec if appropriate; if it is shadowed, it won't be called */
  def shadowsRec(rec: Option[(Name,Arity)], names: Seq[Name]): Option[(Name,Arity)] =
    rec.filterNot(names contains _._1)

  def compileRec(name: Name) = new Arity0(Var(name)) {
    def apply(rec: Rt, r: R) = { r.boxed = rec; 0.0 }
    def bind(env: Map[Name,Rt]) = ()
  }

  /** Actual compile implementation. */
  private[unisonweb]
  def compile(builtins: String => Rt, e0: TermC, boundByCurrentLambda: Option[Set[Name]],
              recursiveVars: Set[Name], currentRec: Option[(Name,Arity)],
              isTail: Boolean): Rt = { val e = unbindRecursiveVars(e0, recursiveVars); e match {
    case Num(n) => compileNum(n)
    case Builtin(name) => builtins(name)
    case Compiled(rt) => rt
    case Var(name) => currentRec match {
      case Some((n,_)) if n == name => compileRec(name)
      // compile a variable as free if it's a recursive var OR
      // we are inside a lambda and this var is bound outside this lambda
      case _ =>
        val compileAsFree = recursiveVars.contains(name) || boundByCurrentLambda.exists(vs => !vs.contains(name))
        compileVar(name, e, compileAsFree)
    }
    case If0(cond,if0,ifNot0) =>
      compileIf0(builtins, e, boundByCurrentLambda, recursiveVars, currentRec, isTail)(cond, if0, ifNot0)
    case Lam(names, body) =>
      compileLambda(builtins, e, boundByCurrentLambda, recursiveVars, currentRec)(names, body)
    case LetRec(bindings, body) => bindings match {
      case (name,f@Lam(vs,bodyf)) :: Nil =>
        compileLetRec1(builtins, e, boundByCurrentLambda, recursiveVars, currentRec, isTail)(name,vs,f,bodyf,body)
      case _ => compileLetRec(builtins, e, boundByCurrentLambda, recursiveVars, currentRec, isTail)(bindings, body)
    }
    case Let1(name, binding, body) => // `let name = binding; body`
      compileLet1(builtins, e, boundByCurrentLambda, recursiveVars, currentRec, isTail)(name, binding, body)
    case Apply(fn, args) =>
      compileFunctionApplication(builtins, e, boundByCurrentLambda, recursiveVars, currentRec, isTail)(fn, args)
  }}

  // thing we want to check for is whether the body of the function references
  // name in tail position, if so, we create a while loop
  def hasTailRecursiveCall(recName: Name, arity: Int, body: TermC): Boolean = false // todo

  def compileNum(n: Double): Rt = new Arity0(Num(n)) {
    override def isEvaluated = true
    def apply(rec: Rt, r: R) = n // todo - need to null out r.boxed?
    def bind(env: Map[Name,Rt]) = ()
  }

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
      case (Var(v), args) if currentRec.contains((v, args.length)) =>
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

  // for tail calls, don't check R.tailCall
  // for non-tail calls, check R.tailCall in a loop

  def decompileSlot(unboxed: D, boxed: Rt): Term =
    if (boxed eq null) Num(unboxed)
    else boxed.decompile

  def toRuntime(unboxed: D, boxed: Rt): Rt =
    if (boxed eq null) compileNum(unboxed)
    else boxed
}
