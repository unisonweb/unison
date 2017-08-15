package org.unisonweb.compilation

import org.unisonweb.Term.Name

package v2 {
  case class Slot(unboxed: D, boxed: Value)
  case class Result(var boxed: Value)
}

package object v2 {
  type V = Value
  type R = Result

  def env(t: TermC): Vector[Name] = t.annotation._2

  def freeVars(t: TermC): Set[Name] = t.annotation._1

  def stackSize(t: TermC): Int = stackSize(freeVars(t), env(t))

  /**
   * Given a set of free variables, and a stack of bound variables, figure out
   * how many elements from `bound` stack we need to be able to resolve all free vars.
   *
   * Ex: Set(x,y) and bound = Vector(x,p,q,r,y,z), arity would be: 5, since we need `bound.take(5)`
   * to have access to both `x` and `y`.
   */
  def stackSize(freeVars: Set[Name], bound: Vector[Name]): Int =
    if (freeVars.isEmpty) 0
    else freeVars.view.map(fv => bound.indexOf(fv)).max + 1

}
