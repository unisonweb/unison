package org.unisonweb

import Runtime._
import Term.{Name,Term}
import annotation.switch

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

  def apply(result: R): Unit

  def apply(arg1: D, arg1b: Rt,
            result: R): Unit

  def apply(arg1: D, arg1b: Rt,
            arg2: D, arg2b: Rt,
            result: R): Unit

  def apply(arg1: D, arg1b: Rt,
            arg2: D, arg2b: Rt,
            arg3: D, arg3b: Rt,
            result: R): Unit

  def apply(arg1: D, arg1b: Rt,
            arg2: D, arg2b: Rt,
            arg3: D, arg3b: Rt,
            arg4: D, arg4b: Rt,
            result: R): Unit

  def apply(args: Array[Slot],
            result: R): Unit

  /**
   * Lambdas containing free vars are handled specially, e.g. in `let k = 42; x -> x + k`
   * the `x -> x + k` closes over `k`, and `freeVarsUnderLambda` would have `k`.
   *
   * Way this is handled is the lambda is created in two stages - first we create `x -> x + k`,
   * noting that `k` is still free. A subsequent call to `bind` will bind `k` to something.
   *
   * This will ALWAYS be empty if `isEvaluated` is true.
   */
  def freeVarsUnderLambda: Set[Name] = Set.empty

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

  case class Result(var unboxed: D = 0.0,
                    var boxed: Rt = null,
                    var tailCall: Rt = null,
                    var tailArg1: D = 0.0,
                    var tailArg1b: Rt = null,
                    var tailArg2: D = 0.0,
                    var tailArg2b: Rt = null,
                    var tailArgs: Array[Slot] = null) {
    final def toRuntime =
      if (boxed eq null) compileNum(unboxed)
      else boxed
  }

  /** Used for representing parameters passed to `Runtime.apply` for large number of parameters. */
  case class Slot(var unboxed: D = 0,
                  var boxed: Rt = null)

  // todo: exception for doing algebraic effects
  case class Yielded(effect: Rt, continuation: Rt) extends Throwable

  /** Constant indicating current term is in tail position, should be compiled accordingly. */
  val IsTail = true

  /** Constant indicating current term not in tail position, should be compiled accordingly. */
  val IsNotTail = false

  /**
   * This is the main public compilation function. Takes a function for resolving builtins, a term,
   * and returns a `Runtime`.
   */
  def compile(builtins: String => Rt)(e: Term): Rt =
    compile(builtins, ABT.annotateBound(e), None, Map(), IsTail)

  /** Compile and evaluate a term, the return result back as a term. */
  def normalize(builtins: String => Rt)(e: Term): Term = {
    val rt = compile(builtins)(e)
    val r = Result()
    eval(rt, r)
    decompileSlot(r.unboxed, r.boxed)
  }
  
  private def unbindRecursiveVars(e: TermC, recursiveVars: Map[Name,TermC]): TermC =
    e.reannotate { case (free,bound) => (free, bound.filterNot(recursiveVars.contains(_))) }

  /** Actual compile implementation. */
  private
  def compile(builtins: String => Rt, e0: TermC, boundByCurrentLambda: Option[Set[Name]],
              recursiveVars: Map[Name,TermC], isTail: Boolean): Rt = { val e = unbindRecursiveVars(e0, recursiveVars); e match {
    case Num(n) => compileNum(n)
    case Builtin(name) => builtins(name)
    case Var(name) =>
      // compile a variable as free if it's a recursive var OR
      // we are inside a lambda and this var is bound outside this lambda
      val compileAsFree = recursiveVars.contains(name) ||
                          boundByCurrentLambda.map(vs => !vs.contains(name)).getOrElse(false)
      compileVar(name, e, compileAsFree)
    case If0(cond,if0,ifNot0) =>
      compileIf0(builtins, e, boundByCurrentLambda, recursiveVars, isTail, cond, if0, ifNot0)
    case Lam(names, body) =>
      compileLambda(builtins, e, Some(names.toSet), recursiveVars -- names)(names, body)
    case LetRec(bindings, body) =>
      compileLetRec(builtins, e, boundByCurrentLambda, recursiveVars, isTail, bindings, body)
    case Let1(name, binding, body) => // `let name = binding; body`
      compileLet1(name, binding, body, builtins, e, boundByCurrentLambda, recursiveVars, isTail)
    case Apply(Builtin(_), args) if isTail =>
      // don't bother with tail calls for builtins; assume they use constant stack
      compile(builtins, e, boundByCurrentLambda, recursiveVars, IsNotTail)
    case Apply(fn, List()) => compile(builtins, fn, boundByCurrentLambda, recursiveVars, isTail)
    case Apply(fn, args) =>
      compileFunctionApplication(builtins, e, boundByCurrentLambda, recursiveVars, isTail, fn, args)
  }}

  def compileIf0(
      builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
      recursiveVars: Map[Name,TermC], isTail: Boolean, cond: TermC, if0: TermC, ifNot0: TermC): Rt = {
    val compiledCond = compile(builtins, cond, boundByCurrentLambda, recursiveVars, IsNotTail)
    val compiledIf0 = compile(builtins, if0, boundByCurrentLambda, recursiveVars, isTail)
    val compiledIfNot0 = compile(builtins, ifNot0, boundByCurrentLambda, recursiveVars, isTail)
    // todo - partial evaluation, if cond has no free vars
    arity(freeVars(e), env(e)) match {
      case 0 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity0(e,()) {
          def apply(r: R) = { eval(cond,r); if (r.unboxed == 0.0) if0(r) else ifNot0(r) }
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case 1 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity1(e,()) {
          def apply(x1: D, x1b: Rt, r: R) = {
            eval(cond,x1,x1b,r)
            if (r.unboxed == 0.0) if0(x1,x1b,r)
            else ifNot0(x1,x1b,r)
          }
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case 2 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity2(e,()) {
          def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
            eval(cond,x1,x1b,x2,x2b,r)
            if (r.unboxed == 0.0) if0(x1,x1b,x2,x2b,r)
            else ifNot0(x1,x1b,x2,x2b,r)
          }
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case 3 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity3(e,()) {
          def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
            eval(cond,x1,x1b,x2,x2b,x3,x3b,r)
            if (r.unboxed == 0.0) if0(x1,x1b,x2,x2b,x3,x3b,r)
            else ifNot0(x1,x1b,x2,x2b,x3,x3b,r)
          }
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case 4 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity4(e,()) {
          def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = {
            eval(cond,x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
            if (r.unboxed == 0.0) if0(x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
            else ifNot0(x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          }
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case n =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends ArityN(n,e,()) {
          def apply(args: Array[Slot], r: R) = {
            evalN(cond,args,r)
            if (r.unboxed == 0.0) if0(args,r)
            else ifNot0(args,r)
          }
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
    }
  }

  def compileFunctionApplication(
      builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
      recursiveVars: Map[Name,TermC], isTail: Boolean, fn: TermC, args: List[TermC]): Rt = {
    /* Four cases to consider:
       1. static (fn already evaluated, known arity), fully-saturated call (correct # args),
          ex `(x -> x) 42`
       2. static partial application, ex `(x y -> x) 42`, need to form closure or specialize
       3. static overapplication, ex `(x -> x) (y -> y) 42` or `id id 42`
       4. dynamic application, ex in `(f x -> f x) id 42`, `f x` is a dynamic application
    */
    val compiledFn = compile(builtins, fn, boundByCurrentLambda, recursiveVars, IsNotTail)
    val compiledArgs = args.view.map(arg => compile(builtins, arg, boundByCurrentLambda, recursiveVars, IsNotTail)).toArray
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
      if (compiledFn.arity == compiledArgs.length) // 1.
        FunctionApplication.staticCall(compiledFn, compiledArgs, unTermC(e), isTail)
      else if (compiledFn.arity > compiledArgs.length) // 2.
        FunctionApplication.staticCall(compiledFn, compiledArgs, unTermC(e), isTail)
      else // 3. (compiledFn.arity < compiledArgs.length)
        ???
    }
    else // 4.
      arity(freeVars(e), env(e)) match {
        case 0 => compiledArgs.length match {
          case 1 => new Arity0(e,()) with FAB {
            val arg = compiledArgs(0)
            def apply(r: R) =
              if (compiledFn.isEvaluated) {
                eval(arg, r)
                compiledFn(r.unboxed, r.boxed, r)
              }
              else {
                eval(compiledFn, r)
                val fn = r.boxed
                eval(arg, r)
                if (fn.arity == 1) fn(r.unboxed, r.boxed, r)
                else if (fn.arity > 1)
                  sys.error("todo - handle partial application here")
                else sys.error("type error, function of arity: " + fn.arity + " applied to 1 argument")
              }
          }
        }
        case 1 => compiledArgs.length match {
          case 1 => new Arity1(e,()) with FAB {
            val arg = compiledArgs(0)
            def apply(x1: D, x1b: Rt, r: R) =
              if (compiledFn.isEvaluated) {
                eval(arg, x1, x1b, r)
                compiledFn(r.unboxed, r.boxed, r)
              }
              else {
                eval(compiledFn, x1, x1b, r)
                val fn = r.boxed
                eval(arg, x1, x1b, r)
                if (fn.arity == 1) fn(r.unboxed, r.boxed, r)
                else if (fn.arity > 1)
                  sys.error("todo - handle partial application here")
                else sys.error("type error, function of arity: " + fn.arity + " applied to 1 argument")
              }
          }
        }
      }
  }

  def compileLetRec(builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
                    recursiveVars: Map[Name,TermC], isTail: Boolean, bindings: List[(Name,TermC)], body: TermC): Rt = {
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
        def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { eval(b, x1, x1b, x2, x2b, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(x1, x1b, x2, x2b, r)
        }
      }
      case 3 => new Arity3(e,()) with B {
        def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { eval(b, x1, x1b, x2, x2b, x3, x3b, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(x1, x1b, x2, x2b, x3, x3b, r)
        }
      }
      case 4 => new Arity4(e,()) with B {
        def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { eval(b, x1, x1b, x2, x2b, x3, x3b, x4, x4b, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(x1, x1b, x2, x2b, x3, x3b, x4, x4b, r)
        }
      }
      case n => new ArityN(n,e,()) with B {
        def apply(args: Array[Slot], r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { evalN(b, args, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(args, r)
        }
      }
      // todo: finish filling in these let rec cases
    }
  }

  def compileLet1(name: Name, binding: TermC, body: TermC,
                  builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
                  recursiveVars: Map[Name,TermC], isTail: Boolean): Rt = {
    val compiledBinding = compile(builtins, binding, boundByCurrentLambda, recursiveVars, IsNotTail)
    val compiledBody = compile(builtins, body, boundByCurrentLambda.map(_ + name), recursiveVars - name, isTail)
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
        def apply(r: R) = {
          eval(compiledBinding, r)
          compiledBody(r.unboxed, r.boxed, r)
        }
      }
      case 1 => new Arity1(e,()) with LB {
        def apply(x1: D, x1b: Rt, r: R) = {
          eval(compiledBinding, x1, x1b, r)
          compiledBody(r.unboxed, r.boxed, x1, x1b, r)
        }
      }
      case 2 => new Arity2(e,()) with LB {
        def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
          eval(compiledBinding, x1, x1b, x2, x2b, r)
          compiledBody(r.unboxed, r.boxed, x1, x1b, x2, x2b, r)
        }
      }
      case 3 => new Arity3(e,()) with LB {
        def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
          eval(compiledBinding, x1, x1b, x2, x2b, x3, x3b, r)
          compiledBody(r.unboxed, r.boxed, x1, x1b, x2, x2b, x3, x3b, r)
        }
      }
      case 4 => new Arity4(e,()) with LB {
        def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = {
          eval(compiledBinding, x1, x1b, x2, x2b, x3, x3b, x4, x4b, r)
          compiledBody(Array(Slot(r.unboxed, r.boxed), Slot(x1, x1b), Slot(x2, x2b), Slot(x3, x3b), Slot(x4, x4b)), r)
        }
      }
      case n => new ArityN(n,e,()) with LB {
        def apply(args: Array[Slot], r: R) = {
          evalN(compiledBinding, args, r)
          compiledBody(Slot(r.unboxed, r.boxed) +: args, r)
        }
      }
    }
  }

  def compileNum(n: Double): Rt = new Arity0(Num(n)) {
    override def isEvaluated = true
    def apply(r: R) = { r.boxed = null; r.unboxed = n } // callee is responsible for nulling out portion of result that's unused
    def bind(env: Map[Name,Rt]) = ()
  }

  def compileVar(name: Name, e: TermC, compileAsFree: Boolean): Rt =
    if (compileAsFree) new Arity0(e,()) {
      var rt: Rt = null
      def apply(r: R) = rt(r)
        // if (rt eq null) throw new InfiniteLoopError(name)
        // todo : possibly try / catch NPEs
      override def freeVarsUnderLambda = if (rt eq null) Set(name) else Set()
      override def bind(env: Map[Name,Rt]) = env.get(name) match {
        case Some(rt2) => rt = rt2
        case _ => () // not an error, just means that some other scope will bind this free var
      }
      // let rec loop = loop; loop
      // let rec ping = pong; pong = ping; ping
      // let rec ping x = pong (x + 1); pong x = ping (x + 1); ping
      override def decompile = if (rt eq null) super.decompile else rt.decompile
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
    def apply(x1: D, x1b: Rt, r: R) = compiledBody(x1, x1b, r)
    override def isEvaluated = true
  }

  class Lambda2(name1: Name, name2: Name, e: => Term, body: => Term, compiledBody: Rt, builtins: String => Rt) extends Arity2(e) {
    var bound: Map[Name,Rt] = Map.empty
    def bind(env: Map[Name,Rt]) = if (env.isEmpty) () else {
      val env2 = env - name1 - name2
      compiledBody.bind(env2)
      bound = bound ++ env2
    }
    override def apply(x1: D, x1b: Rt, r: R) = {
      val rt = toRuntime(x1, x1b)
      Term.betaReduce(name1, Lam(name2)(body))(Compiled(rt)) match {
        case tm@Lam1(name2, body) =>
          val lam = new Lambda1(name2, tm, compile(builtins)(body))
          lam.bind(bound)
          r.boxed = lam
      }
    }
    def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = compiledBody(x1, x1b, x2, x2b, r)
    override def isEvaluated = true
  }

  class Lambda3(name1: Name, name2: Name, name3: Name, e: => Term, body: => Term, compiledBody: Rt, builtins: String => Rt) extends Arity3(e) {
    var bound: Map[Name,Rt] = Map.empty
    def bind(env: Map[Name,Rt]) = if (env.isEmpty) () else {
      val env2 = env - name1 - name2 - name3
      compiledBody.bind(env2)
      bound = bound ++ env2
    }
    override def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
      apply(x2, x2b, r)
      r.boxed(x1, x1b, r) // todo - more direct impl
    }
    override def apply(x1: D, x1b: Rt, r: R) = {
      val rt = toRuntime(x1, x1b)
      Term.betaReduce(name1, Lam(name2, name3)(body))(Compiled(rt)) match {
        case tm@Lam(List(name2, name3), body) =>
          val lam = new Lambda2(name2, name3, tm, body, compile(builtins)(body), builtins)
          lam.bind(bound)
          r.boxed = lam
      }
    }
    def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = compiledBody(x1, x1b, x2, x2b, x3, x3b, r)
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
    override def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
      val rt1 = toRuntime(x1, x1b)
      val rt2 = toRuntime(x2, x2b)
      Term.betaReduce2(name1, name2, Lam(name3, name4)(body))(Compiled(rt2), Compiled(rt1)) match {
        case tm@Lam(List(name3,name4), body) =>
          val lam = new Lambda2(name3, name4, tm, body, compile(builtins)(body), builtins)
          lam.bind(bound)
          r.boxed = lam
      }
    }
    override def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
      val rt1 = toRuntime(x1, x1b)
      val rt2 = toRuntime(x2, x2b)
      val rt3 = toRuntime(x3, x3b)
      Term.betaReduce3(name1, name2, name3, Lam(name4)(body))(Compiled(rt3), Compiled(rt2), Compiled(rt1)) match {
        case tm@Lam(List(name4), body) =>
          val lam = new Lambda1(name4, tm, compile(builtins)(body))
          lam.bind(bound)
          r.boxed = lam
      }
    }
    override def apply(x1: D, x1b: Rt, r: R) = {
      val rt = toRuntime(x1, x1b)
      Term.betaReduce(name1, Lam(name2, name3, name4)(body))(Compiled(rt)) match {
        case tm@Lam(List(name2, name3, name4), body) =>
          val lam = new Lambda3(name2, name3, name4, tm, body, compile(builtins)(body), builtins)
          lam.bind(bound)
          r.boxed = lam
      }
    }
    def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) =
      compiledBody(x1, x1b, x2, x2b, x3, x3b, x4, x4b, r)
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
    override def apply(x1: D, x1b: Rt, r: R) = {
      val rt = toRuntime(x1, x1b)
      Term.betaReduce(names(0), Lam(names.drop(1):_*)(body))(Compiled(rt)) match {
        case tm@Lam(names, body) => names match {
          case List(name1,name2,name3,name4) =>
            val lam = new Lambda4(name1,name2,name3,name4, tm, body, compile(builtins)(body), builtins)
            lam.bind(bound)
            r.boxed = lam
          case _ =>
            val lam = new LambdaN(names.toArray, tm, body, compile(builtins)(body), builtins)
            lam.bind(bound)
            r.boxed = lam
        }
      }
    }
    override def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
      apply(x2, x2b, r)
      r.boxed(x1, x1b, r) // todo - more direct impl
    }
    override def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
      apply(x3, x3b, r)
      r.boxed(x2, x2b, r)
      r.boxed(x1, x1b, r)
    }
    override def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = {
      apply(x4, x4b, r)
      r.boxed(x3, x3b, r)
      r.boxed(x2, x2b, r)
      r.boxed(x1, x1b, r)
    }
    def apply(args: Array[Slot], r: R) =
      if (args.length == names.length) compiledBody(args, r)
      else if (args.length < names.length) {
        var i = args.length
        var rt: Rt = this
        while (i > 0) {
          val slot = args(i-1)
          rt(slot.unboxed, slot.boxed, r)
          rt = r.boxed
          i -= 1
        }
      }
      else sys.error("LambdaN overapplication")

    override def isEvaluated = true
  }

  // todo - do we still need freeVarsUnderLambda?

  def compileLambda(
      builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
      recursiveVars: Map[Name,TermC])(names: List[Name], body: TermC): Rt = {
    def makeCompiledBody = compile(builtins, body, boundByCurrentLambda, recursiveVars, IsTail)
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
          def apply(x1: D, x1b: Rt, r: R) = {
            compiledVar(x1, x1b, r)
            val lam = makeLambda
            lam.bind(bound + (v -> r.toRuntime))
            r.boxed = lam
          }
        }
        case 2 => new Arity2(e,()) with AccumulateBound {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), v, Var(v))) }.toArray
          def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> { vars(i)._2.apply(x1,x1b,x2,x2b,r); r.toRuntime })
              i += 1
            }
            val lam = makeLambda
            lam.bind(bound ++ rts)
            r.boxed = lam
          }
        }
        case 3 => new Arity3(e,()) with AccumulateBound {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), v, Var(v))) }.toArray
          def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> { vars(i)._2.apply(x1,x1b,x2,x2b,x3,x3b,r); r.toRuntime })
              i += 1
            }
            val lam = makeLambda
            lam.bind(bound ++ rts)
            r.boxed = lam
          }
        }
        case 4 => new Arity4(e,()) with AccumulateBound {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), v, Var(v))) }.toArray
          def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> { vars(i)._2.apply(x1,x1b,x2,x2b,x3,x3b,x4,x4b,r); r.toRuntime })
              i += 1
            }
            val lam = makeLambda
            lam.bind(bound ++ rts)
            r.boxed = lam
          }
        }
        case n => new ArityN(n,e,()) with AccumulateBound {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), v, Var(v))) }.toArray
          def apply(args: Array[Slot], r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> { vars(i)._2.apply(args, r); r.toRuntime })
              i += 1
            }
            val lam = makeLambda
            lam.bind(bound ++ rts)
            r.boxed = lam
          }
        }
      }
    }
  }

  @inline def tailCallLoop(r: R): Unit = {
    while (!(r.tailCall eq null)) {
      val fn = r.tailCall
      r.tailCall = null
      (fn.arity : @switch) match {
        case 1 => fn(r.tailArg1, r.tailArg1b, r)
        case 2 => fn(r.tailArg1, r.tailArg1b, r.tailArg2, r.tailArg2b, r)
        case 3 => fn(r.tailArg1, r.tailArg1b, r.tailArg2, r.tailArg2b,
                             r.tailArgs(0).unboxed, r.tailArgs(0).boxed, r)
        case 4 => fn(r.tailArg1, r.tailArg1b, r.tailArg2, r.tailArg2b,
                             r.tailArgs(0).unboxed, r.tailArgs(0).boxed,
                             r.tailArgs(1).unboxed, r.tailArgs(1).boxed, r)
        case n => fn(Array(Slot(r.tailArg1, r.tailArg1b), Slot(r.tailArg2, r.tailArg2b)) ++
                             r.tailArgs, r)
      }
    }
  }

  @inline
  def eval(rt: Rt, r: R): Unit = {
    r.tailCall = null; r.tailArgs = null
    rt(r)
    tailCallLoop(r)
  }
  @inline
  def eval(rt: Rt, x1: D, x2: Rt, r: R): Unit = {
    rt(x1,x2,r)
    tailCallLoop(r)
  }
  @inline
  def eval(rt: Rt, x1: D, x2: Rt, x3: D, x4: Rt, r: R): Unit = {
    rt(x1,x2,x3,x4,r)
    tailCallLoop(r)
  }
  @inline
  def eval(rt: Rt, x1: D, x2: Rt, x3: D, x4: Rt, x5: D, x6: Rt, r: R): Unit = {
    rt(x1,x2,x3,x4,x5,x6,r)
    tailCallLoop(r)
  }
  @inline
  def eval(rt: Rt, x1: D, x2: Rt, x3: D, x4: Rt, x5: D, x6: Rt, x7: D, x8: Rt, r: R): Unit = {
    rt(x1,x2,x3,x4,x5,x6,x7,x8,r)
    tailCallLoop(r)
  }
  @inline
  def evalN(rt: Rt, args: Array[Slot], r: R): Unit = {
    rt(args,r)
    tailCallLoop(r)
  }
  @inline
  def tailCall(fn: Rt, x1: D, x1b: Rt, r: R): Unit = {
    r.tailCall = fn; r.tailArg1 = x1; r.tailArg1b = x1b; r.tailArgs = null
  }
  @inline
  def tailCall(fn: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R): Unit = {
    r.tailCall = fn; r.tailArg1 = x1; r.tailArg1b = x1b; r.tailArg2 = x2; r.tailArg2b = x2b
    r.tailArgs = null
  }
  @inline
  def tailCall(fn: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R): Unit = {
    r.tailCall = fn; r.tailArg1 = x1; r.tailArg1b = x1b; r.tailArg2 = x2; r.tailArg2b = x2b
    r.tailArgs = Array(Slot(x3,x3b))
  }
  @inline
  def tailCall(fn: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R): Unit = {
    r.tailCall = fn; r.tailArg1 = x1; r.tailArg1b = x1b; r.tailArg2 = x2; r.tailArg2b = x2b
    r.tailArgs = Array(Slot(x3,x3b),Slot(x4,x4b))
  }
  @inline
  def tailCall(fn: Rt, args: Array[Slot], r: R): Unit = {
    r.tailCall = fn; r.tailArg1 = args(0).unboxed; r.tailArg1b = args(0).boxed
    r.tailArg2 = args(1).unboxed; r.tailArg2b = args(1).boxed
    r.tailArgs = args.drop(2)
  }

  def lookupVar(i: Int, name: Name, e: Term): Rt = i match {
    case 0 => new Arity1(e) {
      override def apply(arg: D, argb: Rt, result: R): Unit = {
        result.unboxed = arg
        result.boxed = argb
      }
      def bind(env: Map[Name,Rt]) = ()
    }
    case 1 => new Arity2(e) {
      override def apply(x1: D, x2: Rt,
                         arg: D, argb: Rt, result: R): Unit = {
        result.unboxed = arg
        result.boxed = argb
      }
      def bind(env: Map[Name,Rt]) = ()
    }
    case 2 => new Arity3(e) {
      override def apply(x1: D, x2: Rt, x3: D, x4: Rt,
                         arg: D, argb: Rt, result: R): Unit = {
        result.unboxed = arg
        result.boxed = argb
      }
      def bind(env: Map[Name,Rt]) = ()
    }
    case 3 => new Arity4(e) {
      override def apply(x1: D, x2: Rt, x3: D, x4: Rt, x5: D, x6: Rt,
                         arg: D, argb: Rt, result: R): Unit = {
        result.unboxed = arg
        result.boxed = argb
      }
      def bind(env: Map[Name,Rt]) = ()
    }
    case i => new ArityN(i,e) {
      override def apply(args: Array[Slot], result: R): Unit = {
        result.boxed = args(i).boxed
        result.unboxed = args(i).unboxed
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
    def apply(result: R): Unit
    def apply(arg1: D, arg1b: Rt,
              result: R): Unit = apply(result)
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              result: R): Unit = apply(result)
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              arg3: D, arg3b: Rt,
              result: R): Unit = apply(result)
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              arg3: D, arg3b: Rt,
              arg4: D, arg4b: Rt,
              result: R): Unit = apply(result)
    def apply(args: Array[Slot],
              result: R): Unit = apply(result)
  }

  /** A `Runtime` with just 1 abstract `apply` function, which takes 1 arg. */
  abstract class Arity1(decompileIt: => Term) extends Runtime {
    def this(t: TermC, dummy: Unit) = this(unTermC(t))
    def decompile = decompileIt
    def arity: Int = 1
    def apply(result: R): Unit = result.boxed = this
    def apply(arg1: D, arg1b: Rt,
              result: R): Unit
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              result: R): Unit = apply(arg1, arg1b, result)
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              arg3: D, arg3b: Rt,
              result: R): Unit = apply(arg1, arg1b, result)
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              arg3: D, arg3b: Rt,
              arg4: D, arg4b: Rt,
              result: R): Unit = apply(arg1, arg1b, result)
    def apply(args: Array[Slot],
              result: R): Unit = apply(args(0).unboxed, args(0).boxed, result)
  }

  abstract class Arity2(decompileIt: => Term) extends Runtime { self =>
    def this(t: TermC, dummy: Unit) = this(unTermC(t))
    def decompile = decompileIt
    def arity: Int = 2
    def apply(result: R): Unit = result.boxed = this
    def apply(a2: D, a2b: Rt,
              result: R): Unit = ???
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              result: R): Unit
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              arg3: D, arg3b: Rt,
              result: R): Unit = apply(arg1, arg1b, arg2, arg2b, result)
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              arg3: D, arg3b: Rt,
              arg4: D, arg4b: Rt,
              result: R): Unit = apply(arg1, arg1b, arg2, arg2b, result)
    def apply(args: Array[Slot],
              result: R): Unit = apply(args(0).unboxed, args(0).boxed, args(1).unboxed, args(1).boxed, result)
  }

  abstract class Arity3(decompileIt: => Term) extends Runtime { self =>
    def this(t: TermC, dummy: Unit) = this(unTermC(t))
    def decompile = decompileIt
    def arity: Int = 3
    def apply(result: R): Unit = result.boxed = this
    def apply(a3: D, a3b: Rt,
              result: R): Unit = ???
    def apply(a2: D, a2b: Rt,
              a3: D, a3b: Rt,
              result: R): Unit = ???
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              arg3: D, arg3b: Rt,
              result: R): Unit
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              arg3: D, arg3b: Rt,
              arg4: D, arg4b: Rt,
              result: R): Unit = apply(arg1, arg1b, arg2, arg2b, arg3, arg3b, result)
    def apply(args: Array[Slot],
              result: R): Unit =
              apply(args(0).unboxed, args(0).boxed,
                    args(1).unboxed, args(1).boxed,
                    args(2).unboxed, args(2).boxed, result)
  }

  abstract class Arity4(decompileIt: => Term) extends Runtime { self =>
    def this(t: TermC, dummy: Unit) = this(unTermC(t))
    def decompile = decompileIt
    def arity: Int = 4
    def apply(result: R): Unit = result.boxed = this
    def apply(a4: D, a4b: Rt,
              result: R): Unit = ???
    def apply(a3: D, a3b: Rt,
              a4: D, a4b: Rt,
              result: R): Unit = ???
    def apply(a2: D, a2b: Rt,
              a3: D, a3b: Rt,
              a4: D, a4b: Rt,
              result: R): Unit = ???
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              arg3: D, arg3b: Rt,
              arg4: D, arg4b: Rt,
              result: R): Unit
    def apply(args: Array[Slot],
              result: R): Unit =
              apply(args(0).unboxed, args(0).boxed,
                    args(1).unboxed, args(1).boxed,
                    args(2).unboxed, args(2).boxed,
                    args(3).unboxed, args(3).boxed,
                    result)
  }

  abstract class ArityN(val arity: Int, decompileIt: => Term) extends Runtime { self =>
    def this(arity: Int, t: TermC, dummy: Unit) = this(arity, unTermC(t))
    def decompile = decompileIt
    def apply(result: R): Unit = result.boxed = this
    def apply(aN: D, aNb: Rt,
              result: R): Unit = ???
    def apply(aN_1: D, aN_1b: Rt,
              aN: D, aNb: Rt,
              result: R): Unit = ???
    def apply(aN_2: D, aN_2b: Rt,
              aN_1: D, aN_1b: Rt,
              aN: D, aNb: Rt,
              result: R): Unit = ???
    def apply(aN_3: D, aN_3b: Rt,
              aN_2: D, aN_2b: Rt,
              aN_1: D, aN_1b: Rt,
              aN: D, aNb: Rt,
              result: R): Unit = ???
    def apply(args: Array[Slot],
              result: R): Unit
  }
}
