package org.unisonweb

import compilation2._
import Term.{Name,Term}
import org.unisonweb.util.Sequence

/* Sketch of convenience functions for constructing builtin functions. */
object Builtins {

  // Sequence.snoc : forall a . Sequence a -> a -> Sequence a
  // Sequence.snoc [] 42

  //
  // naming convention
  //   - fb_b is a function taking 1 boxed arg, returning 1 boxed result
  //   - fbu_b is a function taking 1 boxed arg, 1 unboxed arg, returning a boxed result
  //   - fuu_u is a function taking 2 unboxed args, returning an unboxed result
  val builtins = Map(
    fbb_b("Sequence.snoc", "seq", "v", (seq: Sequence[Value], v: Value) => seq :+ v),
    fuu_u("+", "x", "y", (x,y) => x + y)
  )

  abstract class External(val get: Any) extends Value { def toResult(r: R) = { r.boxed = this; U0 } }

  // abstract class BuiltinLambda
  // thoughts -
  //   - interface seems nice, but unclear how handle polymorphic functions (ex: Sequence.snoc)
  //     issue is that polymorphic functions can receive boxed or unboxed values
  //   - idea - could convert both boxed and unboxed to uniform (boxed) representation (a la Scala, Haskell)
  //   - idea - could we somehow generate code for both "boxities"?

  // the boxed version
  def fbb_b[A<:AnyRef,B<:AnyRef,C<:AnyRef](name: String, arg1: String, arg2: String, f: (A,B) => C): (Name, Value.Lambda) = {
    val functionBody: Computation =
      (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        val a: A = x1b.toValue match { case e: External => e.get.asInstanceOf[A] }
        val b: B = x0b.toValue match { case e: External => e.get.asInstanceOf[B] }
        val c: C = f(a,b)
        r.boxed = new External(c) { def decompile: Term = ??? }
        U0
      }
    ???
  }

  def fuu_u(name: String, arg1: String, arg2: String, f: FUU_U): (Name, Value.Lambda) = ???

  // the unboxed version
  abstract class FUU_U { def apply(u1: U, u2: U): U }
}


