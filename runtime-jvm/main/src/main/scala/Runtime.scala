package org.unisonweb

import Runtime._
import Term.{Name,Term}

abstract class Runtime {

  /**
   * If `isEvaluated` is true, arity of 0 is a constant, 1 is unary fn, etc.
   *
   * If `isEvaluated` is false, arity is number of elements needed from stack of
   * free variables in scope that must be passed to `apply` to produce an evaluated result.
   */
  def arity: Int

  /** True if this `Runtime` represents an expression in normal form, such
   *  as a lambda, a constant, or a data constructor. */
  def isEvaluated: Boolean = false

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

  def freeVarsUnderLambda: Set[Name] = Set.empty

  def bind(env: Map[Name, Rt]): Unit = ()

  def decompile: Term
}

object Runtime {

  type D = Double
  type Rt = Runtime
  type R = Result
  type TermC = ABT.AnnotatedTerm[Term.F, (Set[Name], Vector[Name])]
  def unTermC(t: TermC): Term = t.map(_._1)
  def env(t: TermC): Vector[Name] = t.annotation._2
  def freeVars(t: TermC): Set[Name] = t.annotation._1
  def arity(freeVars: Set[Name], bound: Vector[Name]): Int =
    if (freeVars.isEmpty) 0
    else freeVars.view.map(fv => bound.indexOf(fv)).max + 1

  import Term.{freeVars => _, _}

  case class Result(var unboxed: D = 0.0,
                    var boxed: Rt = null,
                    var tailCall: Rt = null,
                    var tailArgs: Array[Slot] = null) {
    final def toRuntime =
      if (boxed eq null) compileNum(unboxed)
      else boxed
  }

  case class Slot(var unboxed: D = 0,
                  var boxed: Rt = null)

  case class Yielded(effect: Rt, continuation: Rt) extends Throwable

  val IsTail = true
  val IsNotTail = false

  def compile(builtins: String => Rt)(e: Term): Rt =
    compile(builtins, ABT.annotateBound(e), None, Map(), IsTail)

  def compile(builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
              recursiveVars: Map[Name,TermC], isTail: Boolean): Rt = e match {
    case Num(n) => compileNum(n)
    case Builtin(name) => builtins(name)
    case Var(name) => compileVar(name, e, boundByCurrentLambda)
    case Lam(names, body) =>
      compileLambda(builtins, e, Some(names.toSet), recursiveVars -- names)(names, body)
    case LetRec(bindings, body) =>
      // add to recursive vars
      // compile all the bindings and the body
      // to evaluate, evaluate all the bindings, getting back a `Rt` for each
      // then call bind on each
      ???
    case Let1(name, binding, body) =>
      val compiledBinding = compile(builtins, binding, boundByCurrentLambda, recursiveVars, IsNotTail)
      val compiledBody = compile(builtins, body, boundByCurrentLambda.map(_ + name), recursiveVars - name, isTail)
      arity(freeVars(e), env(e)) match {
        case 0 => new Arity0(e,()) { def apply(r: R) = { eval0(compiledBinding,r); compiledBody(r.unboxed, r.boxed, r) } }
        case 1 => new Arity1(e,()) {
          def apply(x1: D, x1b: Rt, r: R) = {
            eval1(compiledBinding, x1, x1b, r)
            compiledBody(r.unboxed, r.boxed, x1, x1b, r)
          }
        }
      }
    // todo: finish Apply, Lambda, Let, LetRec
  }

  def compileNum(n: Double): Rt = new Arity0(Num(n)) {
    override def isEvaluated = true
    def apply(r: R) = { r.boxed = null; r.unboxed = n }
  }

  def compileVar(name: Name, e: TermC, boundByCurrentLambda: Option[Set[Name]]): Rt =
    boundByCurrentLambda match {
      // if we're under a lambda, and the variable is bound outside the lambda,
      // we keep it as an unbound variable, to be bound later
      case Some(bound) if !bound.contains(name) =>
        new Arity0(e,()) {
          var rt: Rt = null
          def apply(r: R) = rt(r)
          override val freeVarsUnderLambda = if (rt eq null) Set(name) else Set()
          override def bind(env: Map[Name,Rt]) = env.get(name) match {
            case Some(rt2) => rt = rt2
            case _ => ()
          }
          override def decompile = if (rt eq null) super.decompile else rt.decompile
        }
      case _ => // either not under lambda, or bound locally; compile normally by pulling from env
        env(e).indexOf(name) match {
          case -1 => sys.error("unknown variable: " + name)
          case i => lookupVar(i, unTermC(e))
        }
    }

  trait NF { self: Rt => override def isEvaluated = true }

  def compileLambda(
      builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
      recursiveVars: Map[Name,TermC])(names: List[Name], body: TermC): Rt = {
    val compiledBody = compile(builtins, body, boundByCurrentLambda, recursiveVars, IsTail)
    if (freeVars(e).isEmpty) names.length match {
      case 1 => new Arity1(e,()) with NF { def apply(x1: D, x1b: Rt, r: R) = compiledBody(x1, x1b, r) }
      case 2 => new Arity2(e,()) with NF { def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = compiledBody(x1, x1b, x2, x2b, r) }
      case 3 => new Arity3(e,()) with NF { def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = compiledBody(x1, x1b, x2, x2b, x3, x3b, r) }
      case 4 => new Arity4(e,()) with NF { def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = compiledBody(x1, x1b, x2, x2b, x3, x3b, x4, x4b, r) }
      case n => new ArityN(n,e,()) with NF { def apply(xs: Array[Slot], r: R) = compiledBody(xs, r) }
    }
    else {
      trait Closure { self: Rt =>
        var bound: List[(Name,Rt)] = List()
        override def bind(env: Map[Name,Rt]) =
          if (freeVars(e).exists(v => env.contains(v))) {
            compiledBody.bind(env)
            bound = bound ++ env
          }
          else ()
        override def freeVarsUnderLambda = compiledBody.freeVarsUnderLambda
        override def decompile = {
          /* When decompiling closure, bound vars in environment get substituted into
             lambda body, for instance:

               let { incr x = x + 1; x -> incr x }

             The `x -> incr x` would get decompiled to `x -> (x -> x + 1) x`,
             with `incr` substituted in.

             Care must be taken since some of the bound variables may refer to
             themselves:

               let rec { ping x = pong x; pong x = ping (x + 1); ping }
          */
          // todo: think about whether this is correct, am concerned that
          // there could be some variable capture issues
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
      trait L2 { self: Rt =>
        // avoid binding variables that are locally bound
        override def bind(env: Map[Name,Rt]) = {
          val env2 = env -- locallyBound
          if (env2.isEmpty || !freeVarsUnderLambda.exists(env2.contains(_))) ()
          else compiledBody.bind(env2)
        }
        override def freeVarsUnderLambda = compiledBody.freeVarsUnderLambda
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
  def eval0(rt: Rt, r: R): Unit = {
    rt(r) // todo - interpret tail calls
  }
  @inline
  def eval1(rt: Rt, x1: D, x2: Rt, r: R): Unit = {
    rt(x1,x2,r) // todo - interpret tail calls
  }
  @inline
  def eval2(rt: Rt, x1: D, x2: Rt, x3: D, x4: Rt, r: R): Unit = {
    rt(x1,x2,x3,x4,r) // todo - interpret tail calls
  }
  @inline
  def eval3(rt: Rt, x1: D, x2: Rt, x3: D, x4: Rt, x5: D, x6: Rt, r: R): Unit = {
    rt(x1,x2,x3,x4,x5,x6,r) // todo - interpret tail calls
  }
  @inline
  def eval4(rt: Rt, x1: D, x2: Rt, x3: D, x4: Rt, x5: D, x6: Rt, x7: D, x8: Rt, r: R): Unit = {
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

  abstract class Arity0(decompileIt: Term) extends Runtime {
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

  abstract class Arity1(decompileIt: Term) extends Runtime {
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

  abstract class Arity2(val decompileIt: Term) extends Runtime {
    def this(t: TermC, dummy: Unit) = this(unTermC(t))
    def decompile = decompileIt
    def arity: Int = 2
    def apply(result: R): Unit = result.boxed = this
    def apply(arg1: D, arg1b: Rt,
              result: R): Unit = sys.error("partially apply arity 2")
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

  abstract class Arity3(val decompileIt: Term) extends Runtime {
    def this(t: TermC, dummy: Unit) = this(unTermC(t))
    def decompile = decompileIt
    def arity: Int = 3
    def apply(result: R): Unit = result.boxed = this
    def apply(arg1: D, arg1b: Rt,
              result: R): Unit = sys.error("partially apply arity 3")
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              result: R): Unit = sys.error("partially apply arity 3")
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

  abstract class Arity4(val decompileIt: Term) extends Runtime {
    def this(t: TermC, dummy: Unit) = this(unTermC(t))
    def decompile = decompileIt
    def arity: Int = 4
    def apply(result: R): Unit = result.boxed = this
    def apply(arg1: D, arg1b: Rt,
              result: R): Unit = sys.error("partially apply arity 4")
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              result: R): Unit = sys.error("partially apply arity 4")
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              arg3: D, arg3b: Rt,
              result: R): Unit = sys.error("partially apply arity 4")
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

  abstract class ArityN(val arity: Int, val decompileIt: Term) extends Runtime {
    def this(arity: Int, t: TermC, dummy: Unit) = this(arity, unTermC(t))
    def decompile = decompileIt
    def apply(result: R): Unit = result.boxed = this
    def apply(arg1: D, arg1b: Rt,
              result: R): Unit = sys.error("partially apply arity N")
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              result: R): Unit = sys.error("partially apply arity N")
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              arg3: D, arg3b: Rt,
              result: R): Unit = sys.error("partially apply arity N")
    def apply(arg1: D, arg1b: Rt,
              arg2: D, arg2b: Rt,
              arg3: D, arg3b: Rt,
              arg4: D, arg4b: Rt,
              result: R): Unit = sys.error("partially apply arity N")
    def apply(args: Array[Slot],
              result: R): Unit
  }
}
