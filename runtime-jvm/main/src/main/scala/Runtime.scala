package org.unisonweb

import Runtime._
import Term.{Name,Term}

abstract class Runtime {

  /** The arity of this compiled expression:
   *    0 if a constant, 1 if unary function, etc. */
  def arity: Int
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

  def decompile: Term
}

object Runtime {

  type D = Double
  type Rt = Runtime
  type R = Result

  import Term._

  case class Result(var unboxed: D = 0.0,
                    var boxed: Rt = null,
                    var tailCall: Rt = null,
                    var tailArgs: Array[Slot] = null)

  case class Slot(var unboxed: D = 0,
                  var boxed: Rt = null)

  case class Yielded(effect: Rt, continuation: Rt) extends Throwable

  val IsTail = true
  val IsNotTail = false

  def compile(builtins: String => Rt)(e: Term): Rt = {
    def go(e: Term, env: Vector[Name], isTail: Boolean): Rt = {
      e match {
        case Var(name) => env.indexOf(name) match {
          case -1 => sys.error("unknown variable: " + name)
          case i => lookupVar(i, e)
        }
        case Builtin(name) => builtins(name)
        case Num(n) => new Arity0(e) { def apply(r: R) = r.unboxed = n }
        case Apply(fn, args) =>
          val compiledFn = go(fn, env, IsNotTail)
          val compiledArgs = args.map(go(_, env, IsNotTail)).toArray
          // 4 cases:
          //   dynamic call (first eval, to obtain function of known arity, then apply)
          //   static call fully saturated (apply directly)
          //   static call overapplied (apply to correct arity, then tail call with remaining args)
          //   static call underapplied (form closure or specialize for applied args)
          sys.error("todo"): Rt
        // case Apply(fn, args) => ??? // evaluates fn and all args before returning tail call -
        // non tail-calls need to catch `Yielded` and add to continuation
        // case Handle(handler, block) => just catches Yielded exception in a loop, calls apply1
        // case Yield(term) => throws Yielded with compiled version of term and current continuation
        // linear handlers like state can be pushed down, handled "in place"
        // thus don't have to worry about variable slot movement
        case If0(num, is0, not0) =>
          val compiledNum = go(num, env, IsNotTail)
          val compiledIs0 = go(is0, env, isTail)
          val compiledNot0 = go(not0, env, isTail)
          val arity = compiledNum.arity max compiledIs0.arity max compiledNot0.arity
          (arity: @annotation.switch) match {
            case 0 => new Arity0(e) { def apply(r: R) = {
              compiledNum(r)
              if (r.unboxed == 0.0) compiledIs0(r)
              else compiledNot0(r)
            }}
            case 1 => new Arity1(e) { def apply(x1: D, x2: Rt, r: R) = {
              compiledNum(x1,x2,r)
              if (r.unboxed == 0.0) compiledIs0(x1,x2,r)
              else compiledNot0(x1,x2,r)
            }}
            case 2 => new Arity2(e) { def apply(x1: D, x2: Rt, x3: D, x4: Rt, r: R) = {
              compiledNum(x1,x2,x3,x4,r)
              if (r.unboxed == 0.0) compiledIs0(x1,x2,x3,x4,r)
              else compiledNot0(x1,x2,x3,x4,r)
            }}
            case 3 => new Arity3(e) { def apply(x1: D, x2: Rt, x3: D, x4: Rt, x5: D, x6: Rt, r: R) = {
              compiledNum(x1,x2,x3,x4,x5,x6,r)
              if (r.unboxed == 0.0) compiledIs0(x1,x2,x3,x4,x5,x6,r)
              else compiledNot0(x1,x2,x3,x4,x5,x6,r)
            }}
            case 4 => new Arity4(e) { def apply(x1: D, x2: Rt, x3: D, x4: Rt, x5: D, x6: Rt, x7: D, x8: Rt, r: R) = {
              compiledNum(x1,x2,x3,x4,x5,x6,x7,x8,r)
              if (r.unboxed == 0.0) compiledIs0(x1,x2,x3,x4,x5,x6,x7,x8,r)
              else compiledNot0(x1,x2,x3,x4,x5,x6,x7,x8,r)
            }}
            case i => new ArityN(i, e) { def apply(args: Array[Slot], r: R) = {
              compiledNum(args,r)
              if (r.unboxed == 0.0) compiledIs0(args,r)
              else compiledNot0(args,r)
            }}
          }
      }
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
    go(e, Vector(), IsTail)
  }
  // for tail calls, don't check R.tailCall
  // for non-tail calls, check R.tailCall in a loop

  abstract class Arity0(val decompile: Term) extends Runtime {
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

  abstract class Arity1(val decompile: Term) extends Runtime {
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

  abstract class Arity2(val decompile: Term) extends Runtime {
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

  abstract class Arity3(val decompile: Term) extends Runtime {
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

  abstract class Arity4(val decompile: Term) extends Runtime {
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

  abstract class ArityN(val arity: Int, val decompile: Term) extends Runtime {
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
