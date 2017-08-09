package org.unisonweb.compilation

object selfTailCall {
  @inline
  def apply(x1: D, x1b: Rt, r: R): D =
    throw new SelfCall(x1, x1b, 0.0, null, null)
  @inline
  def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, r: R): D =
    throw new SelfCall(x1, x1b, x2, x2b, null)
  @inline
  def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R): D =
    throw new SelfCall(x1, x1b, x2, x2b, Array(Slot(x3,x3b)))
  @inline
  def apply(x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R): D =
    throw new SelfCall(x1, x1b, x2, x2b, Array(Slot(x3,x3b), Slot(x4,x4b)))
  @inline
  def apply(args: Array[Slot], r: R): D =
    throw new SelfCall(args(0).unboxed, args(0).boxed, args(1).unboxed, args(1).boxed, args.drop(2))
}

object tailCall {
  @inline
  def apply(fn: Rt, x1: D, x1b: Rt, r: R): D =
    throw new TailCall(fn, x1, x1b, 0.0, null, null)
  @inline
  def apply(fn: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R): D =
    throw new TailCall(fn, x1, x1b, x2, x2b, null)
  @inline
  def apply(fn: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R): D =
    throw new TailCall(fn, x1, x1b, x2, x2b, Array(Slot(x3,x3b)))
  @inline
  def apply(fn: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R): D =
    throw new TailCall(fn, x1, x1b, x2, x2b, Array(Slot(x3,x3b), Slot(x4,x4b)))
  @inline
  def apply(fn: Rt, args: Array[Slot], r: R): D =
    throw new TailCall(fn, args(0).unboxed, args(0).boxed, args(1).unboxed, args(1).boxed, args.drop(2))
}