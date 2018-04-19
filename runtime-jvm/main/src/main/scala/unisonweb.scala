package org

import org.unisonweb.Term.Name

package object unisonweb {
  type U = Long // unboxed values
  val U0: U = 0l
  val UTrue: U = 1l
  val UFalse: U = U0

  type B = Param // boxed values
  type R = compilation.Result

  sealed trait Id

  object Id {
    def apply(n: Name): Id = Builtin(n)
    def apply(h: Hash): Id = HashRef(h)

    case class Builtin(name: Term.Name) extends Id
    case class HashRef(hash: Hash) extends Id
  }

  case class ConstructorId(toInt: Int) extends AnyVal

  @inline def boolToUnboxed(b: Boolean): U = if (b) UTrue else UFalse
  @inline def unboxedToBool(u: U): Boolean = u == UTrue

  @inline def longToUnboxed(l: Long): U = l
  @inline def unboxedToLong(u: U): Long = u

  @inline def intToUnboxed(i: Int): U = i
  @inline def unboxedToInt(u: U): Int = u.toInt

  @inline def unboxedToDouble(u: U) = java.lang.Double.longBitsToDouble(u)
  @inline def doubleToUnboxed(d: Double): U = java.lang.Double.doubleToRawLongBits(d)
}
