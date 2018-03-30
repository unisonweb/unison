package org.unisonweb.util

import java.util.IdentityHashMap
import scala.collection.immutable.LongMap

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
    def put(bs: Array[Byte]): Unit
    def putByte(b: Byte): Unit
    def putInt(n: Int): Unit
    def putLong(n: Long): Unit
    def putDouble(n: Double): Unit
    def position: Long
  }

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
          buf.putByte(RefMarker)
          if (includeRefMetadata) {
            buf.putByte(RefMetadata)
            buf.putInt(r.bytePrefix.length)
            buf.put(r.bytePrefix)
          }
          else buf.putByte(RefNoMetadata)
          go(r.dereference)
        }
        else {
          buf.putByte(RefSeenMarker.toByte)
          buf.putLong(pos)
        }
    }
    go(_)
  }

  trait Source {
    def get(n: Int): Array[Byte]
    def getByte: Byte
    def getInt: Int
    def getLong: Long
    def getDouble: Double
    def position: Long
  }

  final val NestedStartMarker = 0
  final val NestedEndMarker = 1
  final val SeenMarker = 2
  final val RefMarker = 3
  final val RefSeenMarker = 4
  final val RefMetadata = 0
  final val RefNoMetadata = 1

  def decode[A,R<:A](nested: (Array[Byte], Sequence[A]) => A,
                     ref: (Long,Array[Byte]) => R,
                     setRef: (R,A) => Unit)(src: Source): A = {
    case object NestedEnd extends Throwable { override def fillInStackTrace = this }
    var decoded = LongMap.empty[A]

    def read1: A = { val pos = src.position; (src.getByte.toInt: @annotation.switch) match {
      case NestedStartMarker =>
        val a = nested(src.get(src.getInt), readNestedChildren(Sequence.empty))
        decoded = decoded.updated(pos, a)
        a
      case NestedEndMarker => throw NestedEnd
      case SeenMarker => decoded(src.getLong)
      case RefMarker =>
        val r =
          if (src.getByte.toInt == RefMetadata)
            ref(src.position, src.get(src.getInt))
          else
            ref(src.position, Array.empty)
        decoded = decoded.updated(pos, r)
        val a = read1
        setRef(r, a)
        a
      case RefSeenMarker => decoded(src.getLong)
    }}

    def readNestedChildren(buf0: Sequence[A]): Sequence[A] = {
      var buf = buf0
      while (true) {
        try { buf = buf :+ read1 }
        catch { case NestedEnd => return buf }
      }
      buf
    }
    read1
  }

  def toBytes(n: Long): Array[Byte] = {
    val result = new Array[Byte](8)
    var m = n
    var i = 8 - 1; while (i >= 0) {
      result(i) = (m & 0xFF).toByte
      m >>= 8
      i -= 1
    }
    return result;
  }
}

