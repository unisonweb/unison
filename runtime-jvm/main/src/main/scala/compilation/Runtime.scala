package org.unisonweb.compilation

import org.unisonweb.Term.{Name, Term}

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

  def apply(rec: Rt, result: R): D
  def apply(rec: Rt, a1: D, a1b: Rt, result: R): D
  def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, result: R): D
  def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, result: R): D
  def apply(rec: Rt, a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt, result: R): D
  def apply(rec: Rt, as: Array[Slot], result: R): D

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