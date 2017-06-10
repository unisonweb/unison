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
  def bind(env: Map[Name, Rt]): Unit = ()

  def decompile: Term
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
    rt(r)
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
      // we compile a variable as free if it's a recursive var OR we are inside a lambda and this var is bound outside this lambda
      val compileAsFree = recursiveVars.contains(name) ||
                          boundByCurrentLambda.map(vs => !vs.contains(name)).getOrElse(false)
      compileVar(name, e, compileAsFree)
    case Lam(names, body) =>
      compileLambda(builtins, e, Some(names.toSet), recursiveVars -- names)(names, body)
    case LetRec(bindings, body) =>
      compileLetRec(builtins, e, boundByCurrentLambda, recursiveVars, isTail, bindings, body)
    case Let1(name, binding, body) => // `let name = binding; body`
      compileLet1(name, binding, body, builtins, e, boundByCurrentLambda, recursiveVars, isTail)
    case Apply(fn, List()) => compile(builtins, fn, boundByCurrentLambda, recursiveVars, isTail)
    case Apply(fn, args) =>
      // Four cases to consider:
      //   1. static (function is already evaluated, known arity), fully-saturated call (correct number of args), ex `(x -> x) 42`
      //   2. static partial application, ex `(x y -> x) 42`, need to form closure or specialize
      //   3. static overapplication, ex `(x -> x) (y -> y) 42` or `id id 42`
      //   4. dynamic application, ex in `(f x -> f x) id 42`, `f x` is a dynamic application
      val compiledFn = compile(builtins, fn, boundByCurrentLambda, recursiveVars, IsNotTail)
      val compiledArgs = args.view.map(arg => compile(builtins, arg, boundByCurrentLambda, recursiveVars, IsNotTail)).toArray
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
          case 1 => compiledArgs.length match {
            case 1 => new Arity1(e,()) {
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
  }}

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
      override def bind(env: Map[Name,Rt]) = {
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
    arity(freeVars(e), env(e)) match {
      case 0 => new Arity0(e,()) {
        def apply(r: R) = {
          eval(compiledBinding, r)
          compiledBody(r.unboxed, r.boxed, r)
        }
      }
      case 1 => new Arity1(e,()) {
        def apply(x1: D, x1b: Rt, r: R) = {
          eval(compiledBinding, x1, x1b, r)
          compiledBody(r.unboxed, r.boxed, x1, x1b, r)
        }
      }
      case 2 => new Arity2(e,()) {
        def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
          eval(compiledBinding, x1, x1b, x2, x2b, r)
          compiledBody(r.unboxed, r.boxed, x1, x1b, x2, x2b, r)
        }
      }
      case 3 => new Arity3(e,()) {
        def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
          eval(compiledBinding, x1, x1b, x2, x2b, x3, x3b, r)
          compiledBody(r.unboxed, r.boxed, x1, x1b, x2, x2b, x3, x3b, r)
        }
      }
      case 4 => new Arity4(e,()) {
        def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = {
          eval(compiledBinding, x1, x1b, x2, x2b, x3, x3b, x4, x4b, r)
          compiledBody(Array(Slot(r.unboxed, r.boxed), Slot(x1, x1b), Slot(x2, x2b), Slot(x3, x3b), Slot(x4, x4b)), r)
        }
      }
      case n => new ArityN(n,e,()) {
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
  }

  def compileVar(name: Name, e: TermC, compileAsFree: Boolean): Rt =
    if (compileAsFree) new Arity0(e,()) {
      var rt: Rt = null
      def apply(r: R) = rt(r)
      override def freeVarsUnderLambda = if (rt eq null) Set(name) else Set()
      override def bind(env: Map[Name,Rt]) = env.get(name) match {
        case Some(rt2) => rt = rt2
        case _ => () // not an error, just means that some other scope will bind this free var
      }
      override def decompile = if (rt eq null) super.decompile else rt.decompile
    }
    else env(e).indexOf(name) match {
      case -1 => sys.error("unknown variable: " + name)
      case i => lookupVar(i, unTermC(e))
    }

  trait NF { self: Rt => override def isEvaluated = true }

  def compileLambda(
      builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
      recursiveVars: Map[Name,TermC])(names: List[Name], body: TermC): Rt = {
    val compiledBody = compile(builtins, body, boundByCurrentLambda, recursiveVars, IsTail)
    def makeCompiledBody = compile(builtins, body, boundByCurrentLambda, recursiveVars, IsTail)
    if (freeVars(e).isEmpty) names.length match {
      case 1 => new Arity1(e,()) with NF { def apply(x1: D, x1b: Rt, r: R) = compiledBody(x1, x1b, r) }
      case 2 => new Arity2(e,()) with NF {
        def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = compiledBody(x1, x1b, x2, x2b, r)

        // handle partial application by specialization
        override def apply(x1: D, x1b: Rt, r: R) = {
          val compiledBody2 = makeCompiledBody
          compiledBody2.bind(Map(names.head -> toRuntime(x1, x1b)))
          def decompiled = unTermC(e)(decompileSlot(x1, x1b))
          r.boxed = new Arity1(decompiled) {
            def apply(x2: D, x2b: Rt, r: R) = compiledBody2(x2, x2b, 0.0, null, r)
          }
        }
        // todo - need to do same sort of thing everywhere in compileLambda
      }
      case 3 => new Arity3(e,()) with NF { def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = compiledBody(x1, x1b, x2, x2b, x3, x3b, r) }
      case 4 => new Arity4(e,()) with NF { def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = compiledBody(x1, x1b, x2, x2b, x3, x3b, x4, x4b, r) }
      case n => new ArityN(n,e,()) with NF {
        def apply(xs: Array[Slot], r: R) = compiledBody(xs, r)
      }
    }
    else {
      val compiledBody2 = compiledBody // NB workaround for https://issues.scala-lang.org/browse/SI-10036
      trait Closure { self: Rt =>
        var bound: List[(Name,Rt)] = List()
        override def bind(env: Map[Name,Rt]) =
          if (freeVars(e).exists(v => env.contains(v))) {
            compiledBody2.bind(env)
            bound = bound ++ env
          }
          else ()
        override def freeVarsUnderLambda = compiledBody2.freeVarsUnderLambda
        override def decompile = {
          /* When decompiling closure, bound vars in environment get substituted into
             lambda body, for instance:

               let { incr x = x + 1; x -> incr x }

             The `x -> incr x` would get decompiled to `x -> (x -> x + 1) x`,
             with `incr` substituted in.

             Care must be taken since some of the bound variables may refer to
             themselves:

               let rec { ping x = pong x; pong x = ping (x + 1); ping }
               let
                 incr x = x + 1
                 let rec { ping x = pong x; pong x = ping (incr x); x -> ping x }
          */
          // todo: think about whether this is correct, am concerned that
          // there could be some variable capture issues
          // note that bound decompiled terms will have no free vars
          // recursiveVars may have freeVars
          // lam only has the free vars
          // possibly need to take into account order?
          // could have equality and hashing as an effect
          val e2 = e.map(_._1)
          if (freeVars(e).exists(fv => recursiveVars.contains(fv))) {
            val e3 = ABT.substs(recursiveVars.mapValues(unTermC))(e2)
            ABT.substs((bound.toMap -- recursiveVars.keys).mapValues(_.decompile))(e3)
          }
          else
            ABT.substs(bound.toMap.mapValues(_.decompile))(e2)
        }
        override def isEvaluated = true
      }
      def createClosure = names.length match {
        case 1 => new Arity1(e,()) with Closure { def apply(x1: D, x1b: Rt, r: R) = compiledBody(x1, x1b, r) }
        case 2 => new Arity2(e,()) with Closure { def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = compiledBody(x1, x1b, x2, x2b, r) }
        case 3 => new Arity3(e,()) with Closure { def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = compiledBody(x1, x1b, x2, x2b, x3, x3b, r) }
        case 4 => new Arity4(e,()) with Closure { def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = compiledBody(x1, x1b, x2, x2b, x3, x3b, x4, x4b, r) }
        case n => new ArityN(n,e,()) with Closure { def apply(xs: Array[Slot], r: R) = compiledBody(xs, r) }
      }
      val locallyBound = compiledBody.freeVarsUnderLambda.filter(v => !recursiveVars.contains(v))
      val compiledBody3 = compiledBody // NB workaround for https://issues.scala-lang.org/browse/SI-10036
      trait L2 { self: Rt =>
        // avoid binding variables that are locally bound
        override def bind(env: Map[Name,Rt]) = {
          val env2 = env -- locallyBound
          if (env2.isEmpty || !freeVarsUnderLambda.exists(env2.contains(_))) ()
          else compiledBody3.bind(env2)
        }
        override def freeVarsUnderLambda = compiledBody3.freeVarsUnderLambda
      }
      arity(locallyBound, env(e)) match {
        case 0 => createClosure
        case 1 => new Arity1(e,()) with L2 {
          val v = locallyBound.toList.head
          val compiledVar = lookupVar(0, Var(v))
          def apply(x1: D, x1b: Rt, r: R) = {
            compiledVar(x1, x1b, r)
            val lam = createClosure
            lam.bind(Map(v -> r.toRuntime))
            r.boxed = lam
          }
        }
        case 2 => new Arity2(e,()) with L2 {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), Var(v))) }.toArray
          def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> { vars(i)._2.apply(x1,x1b,x2,x2b,r); r.toRuntime })
              i += 1
            }
            val lam = createClosure
            lam.bind(rts); r.boxed = lam
          }
        }
        case 3 => new Arity3(e,()) with L2 {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), Var(v))) }.toArray
          def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> { vars(i)._2.apply(x1,x1b,x2,x2b,x3,x3b,r); r.toRuntime })
              i += 1
            }
            val lam = createClosure
            lam.bind(rts); r.boxed = lam
          }
        }
        case 4 => new Arity4(e,()) with L2 {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), Var(v))) }.toArray
          def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> { vars(i)._2.apply(x1,x1b,x2,x2b,x3,x3b,x4,x4b,r); r.toRuntime })
              i += 1
            }
            val lam = createClosure
            lam.bind(rts); r.boxed = lam
          }
        }
        case n => new ArityN(n,e,()) with L2 {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), Var(v))) }.toArray
          def apply(args: Array[Slot], r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> { vars(i)._2.apply(args, r); r.toRuntime })
              i += 1
            }
            val lam = createClosure
            lam.bind(rts); r.boxed = lam
          }
        }
      }
    }
  }

  @inline
  def eval(rt: Rt, r: R): Unit = {
    rt(r) // todo - interpret tail calls
  }
  @inline
  def eval(rt: Rt, x1: D, x2: Rt, r: R): Unit = {
    rt(x1,x2,r) // todo - interpret tail calls
  }
  @inline
  def eval(rt: Rt, x1: D, x2: Rt, x3: D, x4: Rt, r: R): Unit = {
    rt(x1,x2,x3,x4,r) // todo - interpret tail calls
  }
  @inline
  def eval(rt: Rt, x1: D, x2: Rt, x3: D, x4: Rt, x5: D, x6: Rt, r: R): Unit = {
    rt(x1,x2,x3,x4,x5,x6,r) // todo - interpret tail calls
  }
  @inline
  def eval(rt: Rt, x1: D, x2: Rt, x3: D, x4: Rt, x5: D, x6: Rt, x7: D, x8: Rt, r: R): Unit = {
    rt(x1,x2,x3,x4,x5,x6,x7,x8,r) // todo - interpret tail calls
  }
  @inline
  def evalN(rt: Rt, args: Array[Slot], r: R): Unit = {
    rt(args,r) // todo - interpret tail calls
  }
  @inline
  def tailCall(fn: Rt, x1: D, x1b: Rt, r: R): Unit = {
    r.tailCall = fn; r.tailArgs = Array(Slot(x1,x1b))
  }
  @inline
  def tailCall(fn: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R): Unit = {
    r.tailCall = fn; r.tailArgs = Array(Slot(x1,x1b),Slot(x2,x2b))
  }
  @inline
  def tailCall(fn: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R): Unit = {
    r.tailCall = fn; r.tailArgs = Array(Slot(x1,x1b),Slot(x2,x2b),Slot(x3,x3b))
  }
  @inline
  def tailCall(fn: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R): Unit = {
    r.tailCall = fn; r.tailArgs = Array(Slot(x1,x1b),Slot(x2,x2b),Slot(x3,x3b),Slot(x4,x4b))
  }
  @inline
  def tailCall(fn: Rt, args: Array[Slot], r: R): Unit = {
    r.tailCall = fn; r.tailArgs = args
  }

  def lookupVar(i: Int, e: Term): Rt = i match {
    case 0 => new Arity1(e) {
      override def apply(arg: D, argb: Rt, result: R): Unit = {
        result.unboxed = arg
        result.boxed = argb
      }
    }
    case 1 => new Arity2(e) {
      override def apply(x1: D, x2: Rt,
                         arg: D, argb: Rt, result: R): Unit = {
        result.unboxed = arg
        result.boxed = argb
      }
    }
    case 2 => new Arity3(e) {
      override def apply(x1: D, x2: Rt, x3: D, x4: Rt,
                         arg: D, argb: Rt, result: R): Unit = {
        result.unboxed = arg
        result.boxed = argb
      }
    }
    case 3 => new Arity4(e) {
      override def apply(x1: D, x2: Rt, x3: D, x4: Rt, x5: D, x6: Rt,
                         arg: D, argb: Rt, result: R): Unit = {
        result.unboxed = arg
        result.boxed = argb
      }
    }
    case i => new ArityN(i,e) {
      override def apply(args: Array[Slot], result: R): Unit = {
        result.boxed = args(i).boxed
        result.unboxed = args(i).unboxed
      }
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
              result: R): Unit = {
      val closure = new Arity1(Apply(decompileIt, decompileSlot(a2, a2b))) {
        def apply(a1: D, a1b: Rt, r: R) =
          self(a1, a1b, a2, a2b, r)
        override def isEvaluated = true
      }
      result.boxed = closure
    }
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
              result: R): Unit =
      result.boxed = new Arity2(decompileIt(decompileSlot(a3, a3b))) with NF {
        def apply(a1: D, a1b: Rt,
                  a2: D, a2b: Rt,
                  result: R): Unit =
          self(a1, a1b, a2, a2b, a3, a3b, result)
      }
    def apply(a2: D, a2b: Rt,
              a3: D, a3b: Rt,
              result: R): Unit =
      result.boxed = new Arity1(decompileIt(decompileSlot(a3,a3b), decompileSlot(a2,a2b))) with NF {
        def apply(a1: D, a1b: Rt, result: R) =
          self(a1, a1b, a2, a2b, a3, a3b, result)
      }
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
              result: R): Unit =
      result.boxed = new Arity3(decompileIt(decompileSlot(a4, a4b))) with NF {
        def apply(a1: D, a1b: Rt,
                  a2: D, a2b: Rt,
                  a3: D, a3b: Rt,
                  result: R) =
          self(a1, a1b, a2, a2b, a3, a3b, a4, a4b, result)
      }
    def apply(a3: D, a3b: Rt,
              a4: D, a4b: Rt,
              result: R): Unit =
      result.boxed = new Arity2(decompileIt(decompileSlot(a4, a4b), decompileSlot(a3, a3b))) {
        def apply(a1: D, a1b: Rt,
                  a2: D, a2b: Rt,
                  result: R) =
          self(a1, a1b, a2, a2b, a3, a3b, a4, a4b, result)
      }
    def apply(a2: D, a2b: Rt,
              a3: D, a3b: Rt,
              a4: D, a4b: Rt,
              result: R): Unit =
      result.boxed = new Arity1(decompileIt(decompileSlot(a4, a4b),
                                            decompileSlot(a3, a3b),
                                            decompileSlot(a2, a2b))) with NF {
        def apply(a1: D, a1b: Rt, result: R) =
          self(a1, a1b, a2, a2b, a3, a3b, a4, a4b, result)
      }

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
              result: R): Unit = {
      def decompiled = decompileIt(decompileSlot(aN, aNb))
      if (arity > 5) result.boxed = new ArityN(arity - 1, decompiled) {
        def apply(args: Array[Slot], r: R) = self(args :+ Slot(aN, aNb), r)
      }
      else if (arity == 5) result.boxed = new Arity4(decompiled) {
        def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) =
          self(Array(Slot(a1,a1b), Slot(a2,a2b), Slot(a3,a3b), Slot(a4,a4b), Slot(aN,aNb)), r)
      }
      else
        sys.error("ArityN with arity less than 5: " + arity)
    }
    def apply(aN_1: D, aN_1b: Rt,
              aN: D, aNb: Rt,
              result: R): Unit = {
      def decompiled = decompileIt(decompileSlot(aN, aNb), decompileSlot(aN_1, aN_1b))
      if (arity > 6) result.boxed = new ArityN(arity - 2, decompiled) {
        def apply(args: Array[Slot], r: R) = self(args ++ Array(Slot(aN_1,aN_1b),Slot(aN, aNb)), r)
      }
      else if (arity == 6) result.boxed = new Arity4(decompiled) {
        def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) =
          self(Array(Slot(a1,a1b), Slot(a2,a2b), Slot(a3,a3b), Slot(a4,a4b), Slot(aN_1,aN_1b), Slot(aN,aNb)), r)
      }
      else if (arity == 5) result.boxed = new Arity3(decompiled) {
        def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) =
          self(Array(Slot(a1,a1b), Slot(a2,a2b), Slot(a3,a3b), Slot(aN_1,aN_1b), Slot(aN,aNb)), r)
      }
      else
        sys.error("ArityN with arity less than 5: " + arity)
    }
    def apply(aN_2: D, aN_2b: Rt,
              aN_1: D, aN_1b: Rt,
              aN: D, aNb: Rt,
              result: R): Unit = {
      def decompiled = decompileIt(decompileSlot(aN, aNb), decompileSlot(aN_1, aN_1b), decompileSlot(aN_2, aN_2b))
      (arity : @switch) match {
        case 5 => result.boxed = new Arity2(decompiled) {
          def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) =
            self(Array(Slot(a1,a1b), Slot(a2,a2b), Slot(aN_2,aN_2b), Slot(aN_1,aN_1b), Slot(aN,aNb)), r)
        }
        case 6 => result.boxed = new Arity3(decompiled) {
          def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) =
            self(Array(Slot(a1,a1b), Slot(a2,a2b), Slot(a3,a3b), Slot(aN_2,aN_2b), Slot(aN_1,aN_1b), Slot(aN,aNb)), r)
        }
        case 7 => result.boxed = new Arity4(decompiled) {
          def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) =
            self(Array(Slot(a1,a1b), Slot(a2,a2b), Slot(a3,a3b), Slot(a4,a4b), Slot(aN_2,aN_2b), Slot(aN_1,aN_1b), Slot(aN,aNb)), r)
        }
        case arity =>
          if (arity > 7) result.boxed = new ArityN(arity - 3, decompiled) {
            def apply(args: Array[Slot], r: R) = self(args ++ Array(Slot(aN_2,aN_2b),Slot(aN_1,aN_1b),Slot(aN, aNb)), r)
          }
          else
            sys.error("ArityN with arity less than 5: " + arity)
      }
    }

    def apply(aN_3: D, aN_3b: Rt,
              aN_2: D, aN_2b: Rt,
              aN_1: D, aN_1b: Rt,
              aN: D, aNb: Rt,
              result: R): Unit = {
      def decompiled = decompileIt(decompileSlot(aN, aNb), decompileSlot(aN_1, aN_1b), decompileSlot(aN_2, aN_2b), decompileSlot(aN_3, aN_3b))
      (arity : @switch) match {
        case 5 => result.boxed = new Arity1(decompiled) {
          def apply(a1: D, a1b: Rt, r: R) =
            self(Array(Slot(a1,a1b), Slot(aN_3,aN_3b), Slot(aN_2,aN_2b), Slot(aN_1,aN_1b), Slot(aN,aNb)), r)
        }
        case 6 => result.boxed = new Arity2(decompiled) {
          def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, r: R) =
            self(Array(Slot(a1,a1b), Slot(a2,a2b), Slot(aN_3,aN_3b), Slot(aN_2,aN_2b), Slot(aN_1,aN_1b), Slot(aN,aNb)), r)
        }
        case 7 => result.boxed = new Arity3(decompiled) {
          def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, r: R) =
            self(Array(Slot(a1,a1b), Slot(a2,a2b), Slot(a3,a3b), Slot(aN_3,aN_3b), Slot(aN_2,aN_2b), Slot(aN_1,aN_1b), Slot(aN,aNb)), r)
        }
        case 8 => result.boxed = new Arity4(decompiled) {
          def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, r: R) =
            self(Array(Slot(a1,a1b), Slot(a2,a2b), Slot(a3,a3b), Slot(a4,a4b), Slot(aN_3,aN_3b), Slot(aN_2,aN_2b), Slot(aN_1,aN_1b), Slot(aN,aNb)), r)
        }
        case arity =>
          if (arity > 8) result.boxed = new ArityN(arity - 4, decompiled) {
            def apply(args: Array[Slot], r: R) = self(args ++ Array(Slot(aN_2,aN_3b),Slot(aN_2,aN_2b),Slot(aN_1,aN_1b),Slot(aN, aNb)), r)
          }
          else
            sys.error("ArityN with arity less than 5: " + arity)
      }
    }
    def apply(args: Array[Slot],
              result: R): Unit
  }
}
