package org.unisonweb

import Runtime._
import Term.Term

abstract class Runtime {

  /** The arity of this compiled expression. -1 if unknown, 1 if unary fn, etc. */
  def arity: Int

  def apply0(result: Result): Unit

  def apply1(arg1: Double, arg1b: Runtime,
            result: Result): Unit

  def apply2(arg1: Double, arg1b: Runtime,
             arg2: Double, arg2b: Runtime,
             result: Result): Unit

  def apply3(arg1: Double, arg1b: Runtime,
             arg2: Double, arg2b: Runtime,
             arg3: Double, arg3b: Runtime,
             result: Result): Unit

  def apply4(arg1: Double, arg1b: Runtime,
             arg2: Double, arg2b: Runtime,
             arg3: Double, arg3b: Runtime,
             arg4: Double, arg4b: Runtime,
             result: Result): Unit

  def applyN(args: Array[Slot],
             result: Result): Unit

  /** Given decompiled versions of all free variables, produce decompiled version of this `Runtime`. */
  def decompile(vars: Array[Term]): Term
}

object Runtime {
  case class Result(var unboxed: Double, var boxed: Runtime = null,
                    var tailCall: Runtime = null,
                    var tailArgs: Array[Slot] = null)
  case class Slot(var unboxed: Double, var boxed: Runtime = null)
}
