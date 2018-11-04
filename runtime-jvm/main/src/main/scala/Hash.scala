package org.unisonweb

import org.unisonweb.util.Base58

case class Hash(bytes: Array[Byte]) {
  override def toString: String =
    "#" + bytes.map(b => b.formatted("%02x")).toList.mkString.take(10)

  override def hashCode(): Int = bytes.toSeq.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case obj: Hash => bytes.toSeq.equals(obj.bytes.toSeq)
    case _ => sys.error("completely bomb")
  }
}

object Hash {
  def fromBase58(s: String): Hash = Hash(Base58.decode(s))
}