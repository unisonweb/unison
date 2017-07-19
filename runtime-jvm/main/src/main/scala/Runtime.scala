package org.unisonweb

import Runtime._
import Term.{Name,Term}

abstract class Runtime {

  /** True if this `Runtime` represents an expression in normal form, such
   *  as a lambda with no free variables `x -> x`, a constant `42`, or a data constructor like `Nil`.
   *  False if expression still needs evaluation, eg `1 + 1`. */
  def isEvaluated: Boolean = false

  /**
   * If `isEvaluated` is true, arity of 0 is a constant, 1 is unary fn, etc.
   *
   * If `isEvaluated` is false, arity is number of elements needed from stack of
   * free variables in scope that must be passed to `apply` to produce an evaluated result.
   *
   * For instance, in `x -> (x + x)`, the parenthesized expresion `x + x` will need the top-most
   * variable on the stack (the `x`), in order to produce an evaluated result.
   */
  def arity: Int

  def apply(rec: Rt, result: R): D
  def apply(rec: Rt, a1: D, a1b: Rt, result: R): D
  def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, result: R): D
  def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, result: R): D
  def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, result: R): D
  def apply(rec: Rt, as: Array[Slot], result: R): D

  /** Bind any free variables under lambdas, using the provided environment. */
  def bind(env: Map[Name, Rt]): Unit

  def decompile: Term
  /* Two cases:
       1. The current term, t, represented by this Rt has no free variables.
          In this case, decompile is just `t`.
          Ex: (x -> x) decompiles as is.
       2. The current term, t, DOES have free variables, v1, v2, ...
          In this case, decompile needs to obtain the decompiled form of v1, v2,
          etc, and the substitute these into `t`.
          Ex: `(x -> x + k)`, need to obtain decompiled form of `k`, and subst
            this into the body of the lambda.
          BUT there's a problem - the decompiled form of a variable may refer
            to itself, and that needs to be handled appropriately.
          Ex 2 (silly example): `let rec loop = loop; (x -> x + loop)`
            What should happen when decompiling `x -> x + loop` ?
            What we don't want - infinite expansion
            What we do want is probably:
              `x -> x + (let rec loop = loop; loop)`
              OR maybe `let rec loop = loop; (x -> x + loop)`

          Ex 3: `let rec ping = pong; pong = ping; (x -> x + pong)`
            x -> x + (let rec ping = pong; pong = ping; pong) OR
            let rec ping = pong; pong = ping; (x -> x + pong)
            Same idea as above, but now with mutual recursion
          Ex 4 `(x y -> (p -> p + y))`
          `(x -> (y -> y + x)) (fib 15)` ==> `y -> y + 610`
          `(x -> (y -> y + x)) (fib 15)` ==> `y -> y + (fib 15)` -- WRONG, discards the result of the computation
  */
  // At a high level,
  // def decompile(decompiled: collection.mutable.Map[Name, Lazy[Term]]): Term
  // def decompile(idHashMap: IdentityHashMap[Rt,Lazy[Term]]): Term
}

object Runtime {

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

  /** Constant indicating current term is in tail position, should be compiled accordingly. */
  val IsTail = false

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
  private
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
      compileIf0(builtins, e, boundByCurrentLambda, recursiveVars, currentRec, isTail)(cond, if0, ifNot0)
    case Lam(names, body) =>
      compileLambda(builtins, e, boundByCurrentLambda, recursiveVars, currentRec)(names, body)
    case LetRec(bindings, body) => bindings match {
      case (name,f@Lam(vs,bodyf)) :: Nil =>
        compileLetRec1(builtins, e, boundByCurrentLambda, recursiveVars, currentRec, isTail)(name,vs,f,bodyf,body)
      case _ => compileLetRec(builtins, e, boundByCurrentLambda, recursiveVars, isTail)(bindings, body)
    }
    case Let1(name, binding, body) => // `let name = binding; body`
      compileLet1(builtins, e, boundByCurrentLambda, recursiveVars, currentRec, isTail)(name, binding, body)
    case Apply(fn, args) =>
      compileFunctionApplication(builtins, e, boundByCurrentLambda, recursiveVars, currentRec, isTail)(fn, args)
  }}

  // thing we want to check for is whether the body of the function references
  // name in tail position, if so, we create a while loop
  def hasTailRecursiveCall(recName: Name, arity: Int, body: TermC): Boolean = true // todo

  def compileLetRec1(
      builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
      recursiveVars: Set[Name], currentRec: Option[(Name,Arity)], isTail: Boolean)(
      name: Name, vs: List[Name], f: TermC, bodyf: TermC, body: TermC): Rt =
    if (hasTailRecursiveCall(name, vs.length, bodyf)) {
      val vsv = vs.toVector
      val compiledf = {
        val step = compileLambda(builtins, f, boundByCurrentLambda, recursiveVars + name,
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
      (arity(freeVars(e), env(e))) match {
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

  def compileIf0(
      builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
      recursiveVars: Set[Name], currentRec: Option[(Name,Arity)], isTail: Boolean)(
      cond: TermC, if0: TermC, ifNot0: TermC): Rt = {
    val compiledCond = compile(builtins, cond, boundByCurrentLambda, recursiveVars, currentRec, IsNotTail)
    val compiledIf0 = compile(builtins, if0, boundByCurrentLambda, recursiveVars, currentRec, isTail)
    val compiledIfNot0 = compile(builtins, ifNot0, boundByCurrentLambda, recursiveVars, currentRec, isTail)
    // todo - partial evaluation, if cond has no free vars
    arity(freeVars(e), env(e)) match {
      case 0 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity0(e,()) {
          def apply(rec: Rt, r: R) = if ({ try cond(rec,r) catch { case e: TC => loop(e,r) }} == 0.0) if0(rec,r) else ifNot0(rec,r)
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case 1 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity1(e,()) {
          def apply(rec: Rt, x1: D, x1b: Rt, r: R) =
            if ({ try cond(rec,x1,x1b,r) catch { case e: TC => loop(e,r) }} == 0.0) if0(rec,x1,x1b,r)
            else ifNot0(rec,x1,x1b,r)
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case 2 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity2(e,()) {
          def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) =
            if ({ try cond(rec,x1,x1b,x2,x2b,r) catch { case e: TC => loop(e,r) }} == 0.0) if0(rec,x1,x1b,x2,x2b,r)
            else ifNot0(rec,x1,x1b,x2,x2b,r)
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case 3 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity3(e,()) {
          def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) =
            if ({ try cond(rec,x1,x1b,x2,x2b,x3,x3b,r) catch { case e: TC => loop(e,r) }} == 0.0) if0(rec,x1,x1b,x2,x2b,x3,x3b,r)
            else ifNot0(rec,x1,x1b,x2,x2b,x3,x3b,r)
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case 4 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity4(e,()) {
          def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) =
            if ({ try cond(rec,x1,x1b,x2,x2b,x3,x3b,x4,x4b,r) catch { case e: TC => loop(e,r) }} == 0.0) if0(rec,x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
            else ifNot0(rec,x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case n =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends ArityN(n,e,()) {
          def apply(rec: Rt, args: Array[Slot], r: R) =
            if ({ try cond(rec,args,r) catch { case e: TC => loop(e,r) }} == 0.0) if0(rec,args,r)
            else ifNot0(rec,args,r)
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
    }
  }

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
        StaticCall.staticRecCall(compiledArgs, unTermC(e), isTail)
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
            StaticCall.staticCall(compiledFn, compiledArgs, unTermC(e), isTail)
          else { // 3. overapplication (compiledFn.arity < compiledArgs.length)
            val (passed, extra) = compiledArgs.splitAt(compiledFn.arity)
            val fn2 = StaticCall.staticCall(compiledFn, passed, unTermC(e), isTail)
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

  def compileLetRec(builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
                    recursiveVars: Set[Name], isTail: Boolean)(bindings: List[(Name,TermC)], body: TermC): Rt = ???/*{
    // ex:
    //   let rec
    //     blah = 42
    //     let rec
    //       ping x = pong (x + 1)
    //       pong x = ping (x - 1)
    //       ping blah
    // ping = (let rec ping = ...; pong = ...; ping)
    // todo: am hazy on how we are using recursive vars to decompile
    val recursiveVars2 = recursiveVars ++ bindings.view.map(_._1).map { name =>
      val bindings2 = bindings map { case (name, b) => (name, b map (_._1)) }
      // easiest to compute annotateBound 'locally', then fixup by adding parent scopes bound vars
      val appendEnv = (p: (Set[Name], Vector[Name])) => (p._1, p._2 ++ env(e)) // parent scopes vars appear later in the stack
      (name, ABT.annotateBound(LetRec(bindings2:_*)(Var(name))) map appendEnv)
    }
    val boundByCurrentLambda2 = boundByCurrentLambda map (_ ++ bindings.map(_._1))
    val compiledBindings = bindings.view.map(_._2).map(e => compile(builtins, e, boundByCurrentLambda2, recursiveVars2, IsNotTail)).toArray
    val compiledBody = compile(builtins, body, boundByCurrentLambda2, recursiveVars2, isTail)
    val names = bindings.map(_._1).toArray
    // todo: consider doing something fancy to avoid needing to iterate over compiledBindings at runtime
    // compile all the bindings and the body
    // to evaluate, evaluate all the bindings, getting back a `Rt` for each
    // then call bind on each
    val compiledBody2 = compiledBody // NB workaround for https://issues.scala-lang.org/browse/SI-10036
    val compiledBindings2 = compiledBindings
    val names2 = names
    trait B { self : Rt =>
      def bind(env: Map[Name,Rt]) = {
        // remove any bindings shadowed in local let rec
        val env2 = env -- names2
        if (env2.nonEmpty) {
          compiledBindings2.foreach(_.bind(env2))
          compiledBody2.bind(env2)
        }
      }
    }
    // observation - most of the time, bindings will be lambdas, so doesn't really matter whether
    // evaluation of bindings is super fast
    // might want to 'de-rec' useless let recs since regular let code is going to be faster probably
    arity(freeVars(e), env(e)) match {
      case 0 => new Arity0(e,()) with B {
        def apply(r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { eval(b, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(r)
        }
      }
      case 1 => new Arity1(e,()) with B {
        def apply(x1: D, x1b: Rt, r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { eval(b, x1, x1b, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(x1, x1b, r)
        }
      }
      case 2 => new Arity2(e,()) with B {
        def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { eval(b, x1, x1b, x2, x2b, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(rec, x1, x1b, x2, x2b, r)
        }
      }
      case 3 => new Arity3(e,()) with B {
        def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { eval(b, x1, x1b, x2, x2b, x3, x3b, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(rec, x1, x1b, x2, x2b, x3, x3b, r)
        }
      }
      case 4 => new Arity4(e,()) with B {
        def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { eval(b, x1, x1b, x2, x2b, x3, x3b, x4, x4b, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(rec, x1, x1b, x2, x2b, x3, x3b, x4, x4b, r)
        }
      }
      case n => new ArityN(n,e,()) with B {
        def apply(rec: Rt, args: Array[Slot], r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { evalN(b, args, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(rec, args, r)
        }
      }
    }
  }*/

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
    arity(freeVars(e), env(e)) match {
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

  def compileNum(n: Double): Rt = new Arity0(Num(n)) {
    override def isEvaluated = true
    def apply(rec: Rt, r: R) = n // todo - need to null out r.boxed?
    def bind(env: Map[Name,Rt]) = ()
  }

  def compileVar(name: Name, e: TermC, compileAsFree: Boolean): Rt =
    if (compileAsFree) new Rt {
      var rt: Rt = null
      def arity = rt.arity
      def apply(rec: Rt, r: R) = rt(rec,r)
      def apply(rec: Rt, x1: D, x1b: Rt, r: R) = rt(rec,x1,x1b,r)
      def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = rt(rec,x1,x1b,x2,x2b,r)
      def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = rt(rec,x1,x1b,x2,x2b,x3,x3b,r)
      def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = rt(rec,x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
      def apply(rec: Rt, args: Array[Slot], r: R) = rt(rec,args,r)
        // if (rt eq null) throw new InfiniteLoopError(name)
        // todo : possibly try / catch NPEs
      override def bind(env: Map[Name,Rt]) = env.get(name) match {
        case Some(rt2) => rt = rt2
        case _ => () // not an error, just means that some other scope will bind this free var
      }
      override def isEvaluated = !(rt eq null)
      // let rec loop = loop; loop
      // let rec ping = pong; pong = ping; ping
      // let rec ping x = pong (x + 1); pong x = ping (x + 1); ping
      def decompile = if (rt eq null) unTermC(e) else rt.decompile
    }
    else env(e).indexOf(name) match {
      case -1 => sys.error("unknown variable: " + name)
      case i => lookupVar(i, name, unTermC(e))
    }

  trait NF { self: Rt =>
    override def isEvaluated = true
    def bind(env: Map[Name,Rt]) = ()
  }

  trait AccumulateBound { self : Rt =>
    var bound : Map[Name,Rt] = Map.empty
    def bind(env: Map[Name,Rt]) = bound = env ++ bound
  }

  class Lambda1(name: Name, e: => Term, compiledBody: Rt) extends Arity1(e) {
    def bind(env: Map[Name,Rt]) = compiledBody.bind(env - name)
    def apply(rec: Rt, x1: D, x1b: Rt, r: R) = compiledBody(rec, x1, x1b, r)
    override def isEvaluated = true
  }

  class Lambda2(name1: Name, name2: Name, e: => Term, body: => Term, compiledBody: Rt, builtins: String => Rt) extends Arity2(e) {
    var bound: Map[Name,Rt] = Map.empty
    def bind(env: Map[Name,Rt]) = if (env.isEmpty) () else {
      val env2 = env - name1 - name2
      compiledBody.bind(env2)
      bound = bound ++ env2
    }
    override def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
      val rt = toRuntime(x1, x1b)
      Term.betaReduce(name1, Lam(name2)(body))(Compiled(rt)) match {
        case tm@Lam1(name2, body) =>
          val lam = new Lambda1(name2, tm, compile(builtins)(body))
          lam.bind(bound)
          r.boxed = lam
          0.0
      }
    }
    def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = compiledBody(rec, x1, x1b, x2, x2b, r)
    override def isEvaluated = true
  }

  class Lambda3(name1: Name, name2: Name, name3: Name, e: => Term, body: => Term, compiledBody: Rt, builtins: String => Rt) extends Arity3(e) {
    var bound: Map[Name,Rt] = Map.empty
    def bind(env: Map[Name,Rt]) = if (env.isEmpty) () else {
      val env2 = env - name1 - name2 - name3
      compiledBody.bind(env2)
      bound = bound ++ env2
    }
    override def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
      apply(rec, x2, x2b, r)
      r.boxed(rec, x1, x1b, r) // todo - more direct impl
    }
    override def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
      val rt = toRuntime(x1, x1b)
      Term.betaReduce(name1, Lam(name2, name3)(body))(Compiled(rt)) match {
        case tm@Lam(List(name2, name3), body) =>
          val lam = new Lambda2(name2, name3, tm, body, compile(builtins)(body), builtins)
          lam.bind(bound)
          r.boxed = lam
          0.0
      }
    }
    def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) =
      compiledBody(rec, x1, x1b, x2, x2b, x3, x3b, r)
    override def isEvaluated = true
  }

  class Lambda4(name1: Name, name2: Name, name3: Name, name4: Name,
                e: => Term, body: => Term, compiledBody: Rt, builtins: String => Rt) extends Arity4(e) {
    var bound: Map[Name,Rt] = Map.empty
    def bind(env: Map[Name,Rt]) = if (env.isEmpty) () else {
      val env2 = env - name1 - name2 - name3
      compiledBody.bind(env2)
      bound = bound ++ env2
    }
    override def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
      val rt1 = toRuntime(x1, x1b)
      val rt2 = toRuntime(x2, x2b)
      Term.betaReduce2(name1, name2, Lam(name3, name4)(body))(Compiled(rt2), Compiled(rt1)) match {
        case tm@Lam(List(name3,name4), body) =>
          val lam = new Lambda2(name3, name4, tm, body, compile(builtins)(body), builtins)
          lam.bind(bound)
          r.boxed = lam
          0.0
      }
    }
    override def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
      val rt1 = toRuntime(x1, x1b)
      val rt2 = toRuntime(x2, x2b)
      val rt3 = toRuntime(x3, x3b)
      Term.betaReduce3(name1, name2, name3, Lam(name4)(body))(Compiled(rt3), Compiled(rt2), Compiled(rt1)) match {
        case tm@Lam(List(name4), body) =>
          val lam = new Lambda1(name4, tm, compile(builtins)(body))
          lam.bind(bound)
          r.boxed = lam
          0.0
      }
    }
    override def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
      val rt = toRuntime(x1, x1b)
      Term.betaReduce(name1, Lam(name2, name3, name4)(body))(Compiled(rt)) match {
        case tm@Lam(List(name2, name3, name4), body) =>
          val lam = new Lambda3(name2, name3, name4, tm, body, compile(builtins)(body), builtins)
          lam.bind(bound)
          r.boxed = lam
          0.0
      }
    }
    def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) =
      compiledBody(rec, x1, x1b, x2, x2b, x3, x3b, x4, x4b, r)
    override def isEvaluated = true
  }

  class LambdaN(names: Array[Name], e: => Term, body: => Term, compiledBody: Rt, builtins: String => Rt)
      extends ArityN(names.length, e) {
    var bound: Map[Name,Rt] = Map.empty
    def bind(env: Map[Name,Rt]) = if (env.isEmpty) () else {
      val env2 = env -- names
      compiledBody.bind(env2)
      bound = bound ++ env2
    }
    override def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
      val rt = toRuntime(x1, x1b)
      Term.betaReduce(names(0), Lam(names.drop(1):_*)(body))(Compiled(rt)) match {
        case tm@Lam(names, body) => names match {
          case List(name1,name2,name3,name4) =>
            val lam = new Lambda4(name1,name2,name3,name4, tm, body, compile(builtins)(body), builtins)
            lam.bind(bound)
            r.boxed = lam
            0.0
          case _ =>
            val lam = new LambdaN(names.toArray, tm, body, compile(builtins)(body), builtins)
            lam.bind(bound)
            r.boxed = lam
            0.0
        }
      }
    }
    override def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
      apply(rec, x2, x2b, r)
      r.boxed(rec, x1, x1b, r) // todo - more direct impl
    }
    override def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
      apply(rec, x3, x3b, r)
      r.boxed(rec, x2, x2b, r)
      r.boxed(rec, x1, x1b, r)
    }
    override def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = {
      apply(rec, x4, x4b, r)
      r.boxed(rec, x3, x3b, r)
      r.boxed(rec, x2, x2b, r)
      r.boxed(rec, x1, x1b, r)
    }
    def apply(rec: Rt, args: Array[Slot], r: R) =
      if (args.length == names.length) compiledBody(rec, args, r)
      else if (args.length < names.length) {
        var i = args.length
        var rt: Rt = this
        while (i > 0) {
          val slot = args(i-1)
          rt(rec, slot.unboxed, slot.boxed, r)
          rt = r.boxed
          i -= 1
        }
        0.0
      }
      else sys.error("LambdaN overapplication")

    override def isEvaluated = true
  }

  def compileLambda(
      builtins: String => Rt, e: TermC, boundByCurrentLambda0: Option[Set[Name]],
      recursiveVars0: Set[Name], currentRec0: Option[(Name,Arity)])(names: List[Name], body: TermC): Rt = {
    val boundByCurrentLambda = Some(names.toSet)
    val recursiveVars = recursiveVars0 -- names
    val currentRec = shadowsRec(currentRec0, names)
    def makeCompiledBody = compile(builtins, body, boundByCurrentLambda, recursiveVars, currentRec, IsTail)
    lazy val eUnC = unTermC(e)
    lazy val bodyUnC = unTermC(body)
    def makeLambda = names match {
      case name1 :: tl => tl match {
        case Nil => new Lambda1(name1, eUnC, makeCompiledBody)
        case name2 :: tl => tl match {
          case Nil => new Lambda2(name1, name2, eUnC, bodyUnC, makeCompiledBody, builtins)
          case name3 :: tl => tl match {
            case Nil => new Lambda3(name1, name2, name3, eUnC, bodyUnC, makeCompiledBody, builtins)
            case name4 :: tl => tl match {
              case Nil => new Lambda4(name1, name2, name3, name4, eUnC, bodyUnC, makeCompiledBody, builtins)
              case _ => new LambdaN(names.toArray, eUnC, bodyUnC, makeCompiledBody, builtins)
            }
          }
        }
      }
      case Nil => sys.error("impossible")
    }
    if (freeVars(e).isEmpty) makeLambda
    else {
      val locallyBound = freeVars(body).filter(v => !recursiveVars.contains(v))
      arity(locallyBound, env(e)) match {
        case 0 => makeLambda
        case 1 => new Arity1(e,()) with AccumulateBound {
          val v = locallyBound.toList.head
          val compiledVar = lookupVar(0, v, Var(v))
          def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
            val lam = makeLambda
            lam.bind(bound + (v -> r.toRuntime(compiledVar(rec, x1, x1b, r))))
            r.boxed = lam
            0.0
          }
        }
        case 2 => new Arity2(e,()) with AccumulateBound {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), v, Var(v))) }.toArray
          def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> r.toRuntime(vars(i)._2.apply(rec,x1,x1b,x2,x2b,r)))
              i += 1
            }
            val lam = makeLambda
            lam.bind(bound ++ rts)
            r.boxed = lam
            0.0
          }
        }
        case 3 => new Arity3(e,()) with AccumulateBound {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), v, Var(v))) }.toArray
          def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> r.toRuntime(vars(i)._2.apply(rec,x1,x1b,x2,x2b,x3,x3b,r)))
              i += 1
            }
            val lam = makeLambda
            lam.bind(bound ++ rts)
            r.boxed = lam
            0.0
          }
        }
        case 4 => new Arity4(e,()) with AccumulateBound {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), v, Var(v))) }.toArray
          def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> r.toRuntime(vars(i)._2.apply(rec,x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)))
              i += 1
            }
            val lam = makeLambda
            lam.bind(bound ++ rts)
            r.boxed = lam
            0.0
          }
        }
        case n => new ArityN(n,e,()) with AccumulateBound {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), v, Var(v))) }.toArray
          def apply(rec: Rt, args: Array[Slot], r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> r.toRuntime(vars(i)._2.apply(rec,args, r)))
              i += 1
            }
            val lam = makeLambda
            lam.bind(bound ++ rts)
            r.boxed = lam
            0.0
          }
        }
      }
    }
  }

  def loop(tc0: TailCall, r: R): D = {
    var tc = tc0
    while (!(tc eq null)) {
      val fn = tc.fn
      try {
        return ((fn.arity : @annotation.switch) match {
          case 1 => fn(fn, tc.x1, tc.x1b, r)
          case 2 => fn(fn, tc.x1, tc.x1b, tc.x2, tc.x2b, r)
          case 3 => fn(fn, tc.x1, tc.x1b, tc.x2, tc.x2b, tc.args(0).unboxed, tc.args(0).boxed, r)
          case 4 => fn(fn, tc.x1, tc.x1b, tc.x2, tc.x2b, tc.args(0).unboxed, tc.args(0).boxed, tc.args(1).unboxed, tc.args(1).boxed, r)
          case n => fn(fn, Array(Slot(tc.x1, tc.x1b), Slot(tc.x2, tc.x2b)) ++ tc.args, r)
        })
      }
      catch { case tc2: TailCall => tc = tc2 }
    }
    0.0
  }

  //@inline
  //def eval(rec: Rt, rt: Rt, r: R): D =
  //  rt(rec, r)
  //  // try rt(rec,r)
  //  // catch { case tc: TailCall => tailCallLoop(tc, r) }
  //@inline
  //def eval(rec: Rt, rt: Rt, x1: D, x2: Rt, r: R): D =
  //  rt(rec,x1,x2,r)
  //  //try rt(rec,x1,x2,r)
  //  //catch { case tc: TailCall => tailCallLoop(tc, r) }
  //@inline
  //def eval(rec: Rt, rt: Rt, x1: D, x2: Rt, x3: D, x4: Rt, r: R): D =
  //  rt(rec,x1,x2,x3,x4,r)
  //  //try rt(rec,x1,x2,x3,x4,r)
  //  //catch { case tc: TailCall => tailCallLoop(tc, r) }
  //@inline
  //def eval(rec: Rt, rt: Rt, x1: D, x2: Rt, x3: D, x4: Rt, x5: D, x6: Rt, r: R): D =
  //  rt(rec,x1,x2,x3,x4,x5,x6,r)
  //  //try rt(rec,x1,x2,x3,x4,x5,x6,r)
  //  //catch { case tc: TailCall => tailCallLoop(tc, r) }
  //@inline
  //def eval(rec: Rt, rt: Rt, x1: D, x2: Rt, x3: D, x4: Rt, x5: D, x6: Rt, x7: D, x8: Rt, r: R): D =
  //  rt(rec,x1,x2,x3,x4,x5,x6,r)
  //  // try rt(rec,x1,x2,x3,x4,x5,x6,r)
  //  // catch { case tc: TailCall => tailCallLoop(tc, r) }
  //@inline
  //def evalN(rec: Rt, rt: Rt, args: Array[Slot], r: R): D =
  //  rt(rec,args,r)
  //  // try rt(rec,args,r)
  //  // catch { case tc: TailCall => tailCallLoop(tc, r) }

  @inline
  def selfTailCall(x1: D, x1b: Rt, r: R): D =
    throw new SelfCall(x1, x1b, 0.0, null, null)
  @inline
  def selfTailCall(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R): D =
    throw new SelfCall(x1, x1b, x2, x2b, null)
  @inline
  def selfTailCall(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R): D =
    throw new SelfCall(x1, x1b, x2, x2b, Array(Slot(x3,x3b)))
  @inline
  def selfTailCall(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R): D =
    throw new SelfCall(x1, x1b, x2, x2b, Array(Slot(x3,x3b), Slot(x4,x4b)))
  @inline
  def selfTailCall(args: Array[Slot], r: R): D =
    throw new SelfCall(args(0).unboxed, args(0).boxed, args(1).unboxed, args(1).boxed, args.drop(2))

  @inline
  def tailCall(fn: Rt, x1: D, x1b: Rt, r: R): D =
    throw new TailCall(fn, x1, x1b, 0.0, null, null)
  @inline
  def tailCall(fn: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R): D =
    throw new TailCall(fn, x1, x1b, x2, x2b, null)
  @inline
  def tailCall(fn: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R): D =
    throw new TailCall(fn, x1, x1b, x2, x2b, Array(Slot(x3,x3b)))
  @inline
  def tailCall(fn: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R): D =
    throw new TailCall(fn, x1, x1b, x2, x2b, Array(Slot(x3,x3b), Slot(x4,x4b)))
  @inline
  def tailCall(fn: Rt, args: Array[Slot], r: R): D =
    throw new TailCall(fn, args(0).unboxed, args(0).boxed, args(1).unboxed, args(1).boxed, args.drop(2))

  def lookupVar(i: Int, name: Name, e: Term): Rt = i match {
    case 0 => new Arity1(e) {
      override def apply(rec: Rt, arg: D, argb: Rt, result: R) = {
        if (!(argb eq null)) result.boxed = argb
        arg
      }
      def bind(env: Map[Name,Rt]) = ()
    }
    case 1 => new Arity2(e) {
      override def apply(rec: Rt, x1: D, x2: Rt,
                         arg: D, argb: Rt, result: R) = {
        if (!(argb eq null)) result.boxed = argb
        arg
      }
      def bind(env: Map[Name,Rt]) = ()
    }
    case 2 => new Arity3(e) {
      override def apply(rec: Rt, x1: D, x2: Rt, x3: D, x4: Rt,
                         arg: D, argb: Rt, result: R) = {
        if (!(argb eq null)) result.boxed = argb
        arg
      }
      def bind(env: Map[Name,Rt]) = ()
    }
    case 3 => new Arity4(e) {
      override def apply(rec: Rt, x1: D, x2: Rt, x3: D, x4: Rt, x5: D, x6: Rt,
                         arg: D, argb: Rt, result: R) = {
        if (!(argb eq null)) result.boxed = argb
        arg
      }
      def bind(env: Map[Name,Rt]) = ()
    }
    case i => new ArityN(i,e) {
      override def apply(rec: Rt, args: Array[Slot], result: R) = {
        val s = args(i)
        if (!(s.boxed eq null)) result.boxed = s.boxed
        s.unboxed
      }
      def bind(env: Map[Name,Rt]) = ()
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

  /** A `Runtime` with just 1 abstract `apply` function, which takes no args. */
  abstract class Arity0(decompileIt: => Term) extends Runtime {
    def this(t: TermC, dummy: Unit) = this(unTermC(t))
    def decompile = decompileIt
    def arity: Int = 0
    def apply(rec: Rt, a1: D, a1b: Rt, r: R) = apply(rec, r)
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) = apply(rec, r)
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) = apply(rec, r)
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) =
      apply(rec, r)
    def apply(rec: Rt, as: Array[Slot], r: R) = apply(rec, r)
  }

  /** A `Runtime` with just 1 abstract `apply` function, which takes 1 arg. */
  abstract class Arity1(decompileIt: => Term) extends Runtime {
    def this(t: TermC, dummy: Unit) = this(unTermC(t))
    def decompile = decompileIt
    def arity: Int = 1

    def apply(rec: Rt, r: R) = { r.boxed = this; 0.0 }
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) =
      apply(rec, a1, a1b, r)
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) =
      apply(rec, a1, a1b, r)
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) =
      apply(rec, a1, a1b, r)
    def apply(rec: Rt, as: Array[Slot], r: R) = apply(rec, as(0).unboxed, as(0).boxed, r)
  }

  abstract class Arity2(decompileIt: => Term) extends Runtime { self =>
    def this(t: TermC, dummy: Unit) = this(unTermC(t))
    def decompile = decompileIt
    def arity: Int = 2

    def apply(rec: Rt, r: R): D = { r.boxed = this; 0.0 }
    def apply(rec: Rt, a2: D, a2b: Rt, r: R): D = ???
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) =
      apply(rec, a1, a1b, a2, a2b, r)
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) =
      apply(rec, a1, a1b, a2, a2b, r)
    def apply(rec: Rt, as: Array[Slot], r: R) =
      apply(rec, as(0).unboxed, as(0).boxed, as(1).unboxed, as(1).boxed, r)
  }

  abstract class Arity3(decompileIt: => Term) extends Runtime { self =>
    def this(t: TermC, dummy: Unit) = this(unTermC(t))
    def decompile = decompileIt
    def arity: Int = 3

    def apply(rec: Rt, r: R) = { r.boxed = this; 0.0 }
    def apply(rec: Rt, a1: D, a1b: Rt, r: R): D = ???
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R): D = ???
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) =
      apply(rec, a1, a1b, a2, a2b, a3, a3b, r)
    def apply(rec: Rt, as: Array[Slot], r: R) =
      apply(rec, as(0).unboxed, as(0).boxed, as(1).unboxed, as(1).boxed, as(2).unboxed, as(2).boxed, r)
  }

  abstract class Arity4(decompileIt: => Term) extends Runtime { self =>
    def this(t: TermC, dummy: Unit) = this(unTermC(t))
    def decompile = decompileIt
    def arity: Int = 4

    def apply(rec: Rt, r: R) = { r.boxed = this; 0.0 }
    def apply(rec: Rt, a1: D, a1b: Rt, r: R): D = ???
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R): D = ???
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R): D = ???
    def apply(rec: Rt, as: Array[Slot], r: R) =
      apply(rec, as(0).unboxed, as(0).boxed, as(1).unboxed, as(1).boxed, as(2).unboxed, as(2).boxed, as(3).unboxed, as(3).boxed, r)
  }

  abstract class ArityN(val arity: Int, decompileIt: => Term) extends Runtime { self =>
    def this(arity: Int, t: TermC, dummy: Unit) = this(arity, unTermC(t))
    def decompile = decompileIt

    def apply(rec: Rt, r: R) = { r.boxed = this; 0.0 }
    def apply(rec: Rt, a1: D, a1b: Rt, r: R): D = ???
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, r: R): D = ???
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R): D = ???
    def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R): D = ???
  }
}
