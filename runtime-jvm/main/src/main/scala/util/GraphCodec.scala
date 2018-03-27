package org.unisonweb.util

import java.util.IdentityHashMap

object GraphCodec {

  trait Graphstream {
    def bytePrefix: Array[Byte]
    def foldLeft[B](b: B)(f: (B,Graphstream) => B): B
  }

  object Graphstream {
    trait Nested extends Graphstream
    trait Ref extends Graphstream {
      def dereference: Graphstream
    }
  }

  trait Sink {
    def putInt(n: Int): Unit
    def put(bs: Array[Byte]): Unit
    def putByte(b: Byte): Unit
    def putLong(n: Long): Unit
    def putDouble(n: Double): Unit
    def position: Long
  }

  val NestedStartMarker = 0 : Byte
  val NestedEndMarker = 1 : Byte
  val RefMarker = 2 : Byte

  /** Encode a `Graphstream` and return the number of bytes written. */
  def encode(g: Graphstream)(buf: Sink): Unit = {
    val seen = new IdentityHashMap[Graphstream.Ref,Long]()
    def go(g: Graphstream): Unit = g match {
      case g : Graphstream.Nested =>
        buf.putByte(NestedStartMarker) // indicates a Nested follows
        buf.putInt(g.bytePrefix.length)
        buf.put(g.bytePrefix)
        g.foldLeft(())((_,g) => go(g))
        buf.putByte(NestedEndMarker)
      case r : Graphstream.Ref =>
        val pos = seen.get(r)
        if (seen.get(r) eq null) {
          seen.put(r, buf.position)
          go(r.dereference)
        }
        else {
          val before = buf.position
          buf.putByte(RefMarker)
          buf.putLong(pos)
        }
    }
    go(g)
  }

  // pretty simple - keep a LongMap[Graphstream] mapping positions
  // to decoded Graphstream
  // read first byte, if it's a 0, read a long, lookup in the map
  // otherwise if it's a 1, read the prefix, then read the children, recursively
  def decode(bytes: Sequence[Byte]): Graphstream = ???

  // def decode
  //  def decode[A](make: (Array[Byte], Sequence[A]) => A): Graphstream => A = {

}

