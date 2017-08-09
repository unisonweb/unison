package org.unisonweb

package object compilation {

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

  case class Result(var boxed: Rt = null) {
    def toRuntime(unboxed: D): Rt = if (boxed eq null) compileNum(unboxed) else boxed
  }

  /** Used for representing parameters passed to `Runtime.apply` for large number of parameters. */
  case class Slot(var unboxed: D = 0,
                  var boxed: Rt = null)

  // todo: exception for doing algebraic effects
  case class Yielded(effect: Rt, continuation: Rt) extends Throwable

  /** Constant indicating current term is in tail position, should be compiled accordingly. */
  val IsTail = false

  /** Constant indicating current term not in tail position, should be compiled accordingly. */
  val IsNotTail = false // todo: why are these both `false`

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
    e.reannotate { case (free,bound) => (free, bound.filterNot(recursiveVars.contains(_))) }

  type Arity = Int
  def shadowRec(rec: Option[(Name,Arity)], name: Name): Option[(Name,Arity)] =
    rec match {
      case Some((n,arity)) if n == name => None
      case _ => rec
    }
  def shadowsRec(rec: Option[(Name,Arity)], names: Seq[Name]): Option[(Name,Arity)] =
    rec match {
      case Some((n,arity)) if names.contains(n) => None
      case _ => rec
    }

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
      case _ => val compileAsFree = recursiveVars.contains(name) ||
                    boundByCurrentLambda.map(vs => !vs.contains(name)).getOrElse(false)
                compileVar(name, e, compileAsFree)
    }
    case If0(cond,if0,ifNot0) =>
      compilation.If0.compileIf0(builtins, e, boundByCurrentLambda, recursiveVars, currentRec, isTail)(cond, if0, ifNot0)
    case Lam(names, body) =>
      Lambda.compileLambda(builtins, e, boundByCurrentLambda, recursiveVars, currentRec)(names, body)
    case LetRec(bindings, body) => bindings match {
      case (name,f@Lam(vs,bodyf)) :: Nil =>
        compilation.LetRec1.compileLetRec1(builtins, e, boundByCurrentLambda, recursiveVars, currentRec, isTail)(name,vs,f,bodyf,body)
      case _ => compilation.LetRec.compileLetRec(builtins, e, boundByCurrentLambda, recursiveVars, isTail)(bindings, body)
    }
    case Let1(name, binding, body) => // `let name = binding; body`
      compilation.Let1.compileLet1(builtins, e, boundByCurrentLambda, recursiveVars, currentRec, isTail)(name, binding, body)
    case Apply(fn, args) =>
      compilation.FunctionApplication
        .compileFunctionApplication(builtins, e, boundByCurrentLambda, recursiveVars, currentRec, isTail)(fn, args)
  }}

  // thing we want to check for is whether the body of the function references
  // name in tail position, if so, we create a while loop
  def hasTailRecursiveCall(recName: Name, arity: Int, body: TermC): Boolean = true // todo

  def compileNum(n: Double): Rt = new Arity0(Num(n)) {
    override def isEvaluated = true
    def apply(rec: Rt, r: R) = n // todo - need to null out r.boxed?
    def bind(env: Map[Name,Rt]) = ()
  }

  trait NF { self: Rt =>
    override def isEvaluated = true
    def bind(env: Map[Name,Rt]) = ()
  }

  trait AccumulateBound { self : Rt =>
    var bound : Map[Name,Rt] = Map.empty
    def bind(env: Map[Name,Rt]) = bound = env ++ bound
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
