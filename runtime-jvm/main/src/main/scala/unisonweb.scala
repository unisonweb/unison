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
  @inline def unboxedToBool(u: U): Boolean = u != U0

  @inline def longToUnboxed(l: Long): U = l
  @inline def unboxedToLong(u: U): Long = u

  @inline def intToUnboxed(i: Int): U = i
  @inline def unboxedToInt(u: U): Int = u.toInt

  @inline def unboxedToDouble(u: U) = java.lang.Double.longBitsToDouble(u)
  @inline def doubleToUnboxed(d: Double): U = java.lang.Double.doubleToRawLongBits(d)
}
