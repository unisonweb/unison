package org.unisonweb

case class Hash(bytes: Array[Byte]) extends AnyVal

object Hash {

  /** Compute the Hash for a constructorId of some data type with hash `h`. */
  def constructorId(h: Hash, constructorId: ConstructorId): Hash =
    ???
}
