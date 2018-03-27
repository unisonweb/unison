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
  val RefStartMarker = 2 : Byte
  val RefEndMarker = 3 : Byte
  val SeenMarker = 4 : Byte

  /** Encode a `Graphstream`. `includeRefMetadata` controls whether the `bytePrefix`
   *  of each `Ref` is written to the output as well. */
  def encode(buf: Sink, includeRefMetadata: Boolean): Graphstream => Unit = {
    val seen = new IdentityHashMap[Graphstream,Long]()
    def go(g: Graphstream): Unit = g match {
      case g : Graphstream.Nested =>
        val pos = seen.get(g)
        if (pos eq null) {
          seen.put(g, buf.position)
          buf.putByte(NestedStartMarker) // indicates a Nested follows
          buf.putInt(g.bytePrefix.length)
          buf.put(g.bytePrefix)
          g.foldLeft(())((_,g) => go(g))
          buf.putByte(NestedEndMarker)
        }
        else {
          buf.putByte(SeenMarker)
          buf.putLong(pos)
        }
      case r : Graphstream.Ref =>
        val pos = seen.get(r)
        if (pos eq null) {
          seen.put(r, buf.position)
          buf.putByte(RefStartMarker)
          if (includeRefMetadata) {
            buf.putInt(r.bytePrefix.length)
            buf.put(r.bytePrefix)
          }
          go(r.dereference)
        }
        else {
          val before = buf.position
          buf.putByte(RefEndMarker)
          buf.putLong(pos)
        }
    }
    go(_)
  }

  // pretty simple - keep a LongMap[Graphstream] mapping positions
  // to decoded Graphstream
  // read first byte, if it's a 0, read a long, lookup in the map
  // otherwise if it's a 1, read the prefix, then read the children, recursively
  def decode(bytes: Sequence[Byte]): Graphstream = ???

  // def decode
  //  def decode[A](make: (Array[Byte], Sequence[A]) => A): Graphstream => A = {

}

