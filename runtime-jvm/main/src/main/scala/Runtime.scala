package org.unisonweb

import Runtime._

abstract class Runtime {

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
}

object Runtime {
  case class Result(var unboxed: Double, var boxed: Runtime = null,
                    var tailCall: Runtime = null,
                    var tailArgs: Array[Slot] = null)
  case class Slot(var unboxed: Double, var boxed: Runtime = null)
}
