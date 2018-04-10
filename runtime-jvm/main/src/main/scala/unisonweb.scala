package org

package object unisonweb {
  type U = Long // unboxed values
  val U0: U = 0
  val True: U = 1
  val False: U = 0

  type B = Param // boxed values
  type R = compilation.Result

  case class ConstructorId(toInt: Int) extends AnyVal

  @inline def boolToUnboxed(b: Boolean): U = if (b) True else False
}