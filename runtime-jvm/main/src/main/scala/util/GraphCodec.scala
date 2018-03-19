package org.unisonweb.util

import scala.collection.mutable

object GraphCodec {

  type Position = Long

  // def decode[A](make: (Array[Byte])): A

  case class Skipstream(prefix: Array[Byte], children: Sequence[Skipstream], size: Long)

  def encode[A](expand: A => (Array[Byte], Sequence[A])): A => Skipstream = {
    root => {
      val m = new mutable.HashMap[Pointer[A], Position]
      def ref(pos: Position): Array[Byte] = ??? // todo

      def go(pos: Position, a: A): Skipstream = {
        val ptr = new Pointer(a)
        m.get(ptr) match {
          case None =>
            m.update(ptr, pos)
            val (prefix0, children) = expand(a)
            var pos1 = pos
            Skipstream(prefix0, children.map(a => { val s = go(pos, a); pos1 += s.size; s }), pos1)
          case Some(pos) =>
            Skipstream(ref(pos), Sequence.empty, ???)
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
