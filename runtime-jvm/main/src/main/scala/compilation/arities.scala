package org.unisonweb.compilation

import org.unisonweb.Term.Term

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