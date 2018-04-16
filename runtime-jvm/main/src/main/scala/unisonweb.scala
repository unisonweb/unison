package org

import org.unisonweb.Term.Name

package object unisonweb {
  type U = Long // unboxed values
  val U0: U = 0
  val True: U = 1
  val False: U = 0

  type B = Param // boxed values
  type R = compilation.Result

  sealed trait Id
  case class Builtin(name: Term.Name) extends Id
  case class HashRef(hash: Hash) extends Id

  object Id {
    def apply(n: Name): Id = Builtin(n)
    def apply(h: Hash): Id = HashRef(h)
  }

  case class ConstructorId(toInt: Int) extends AnyVal

  @inline def boolToUnboxed(b: Boolean): U = if (b) True else False
}