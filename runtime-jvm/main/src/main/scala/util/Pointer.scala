package org.unisonweb.util

class Pointer(val ref: AnyRef) {
  override def equals(o: Any) =
    ref eq o.asInstanceOf[Pointer].ref
  override def hashCode: Int =
    System.identityHashCode(ref)
  override def toString = s"@${System.identityHashCode(ref)}="+ref.toString.take(15)
}
