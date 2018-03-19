package org.unisonweb.util

import scala.collection.mutable

object GraphCodec {

  type Position = Long

  // def decode[A](make: (Array[Byte])): A

  def encode[A](expand: A => (Array[Byte], Sequence[A])): A => Sequence[Byte] = {
    root => {
      val m = new mutable.HashMap[Pointer[A], Position]
      def ref(pos: Position): Array[Byte] = ??? // todo
      // emits s.size, followed by the bytes
      def sized(s: Sequence[Byte]): Sequence[Byte] = ???

      def go(pos: Position, a: A): Sequence[Byte] = {
        val ptr = new Pointer(a)
        m.get(ptr) match {
          case None =>
            m.update(ptr, pos)
            val (prefix0, children) = expand(a)
            val prefix = sized(Bytes.viewArray(prefix0))
            var pos1 = pos
            val body = children.foldLeft(prefix) { (buf, a) =>
              pos1 = pos + buf.size
              buf ++ go(pos1, a)
            }
            sized(body)
          case Some(pos) =>
            Bytes.viewArray(ref(pos))
        }
      }
      go(0, root)
    }
  }

  class Pointer[A](val get: A) {
    override def hashCode = System.identityHashCode(get)
    override def equals(p: Any) =
      p.asInstanceOf[Pointer[AnyRef]].get eq get.asInstanceOf[AnyRef]
  }
}
