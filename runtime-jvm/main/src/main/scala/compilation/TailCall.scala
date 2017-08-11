package org.unisonweb.compilation

class TailCall(val fn: Rt, val x1: D, val x1b: Rt, val x2: D, val x2b: Rt, val args: Array[Slot]) extends Throwable {
  override def fillInStackTrace = this
}

class SelfCall(x1: D, x1b: Rt, x2: D, x2b: Rt, args: Array[Slot]) extends TailCall(null,x1,x1b,x2,x2b,args)

