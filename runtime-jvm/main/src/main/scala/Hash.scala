package org.unisonweb

import org.unisonweb.util.Sequence

case class Hash(bytes: Sequence[Byte]) extends AnyVal

object Hash {

  /** Compute the Hash for a constructorId of some data type with hash `h`. */
  def constructorId(h: Hash, constructorId: ConstructorId): Hash =
    ???
}
