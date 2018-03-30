package org.unisonweb.util

import java.util.IdentityHashMap
import scala.collection.immutable.LongMap
import Codecs.{Source,Sink}

object GraphCodec {

  trait Graphstream[G,R<:G] {
    def bytePrefix(graph: G): Array[Byte]
    def foldLeft[B](graph: G)(b: B)(f: (B,G) => B): B
    def foreach(graph: G)(f: G => Unit): Unit

    def isReference(graph: G): Boolean
    def dereference(graph: R): G
    def reference(position: Long, prefix: Array[Byte]): R
    def setReference(ref: R, referent: G): Unit
    def nested(prefix: Array[Byte], children: Sequence[G]): G

    /**
     * Encode a `G` to the given `Sink`.
     * `includeRefMetadata` controls whether the `bytePrefix`
     * of each `g: G` which passes `isReference(g)` is written
     * to the output as well.
     */
    def encode(buf: Sink, includeRefMetadata: Boolean): G => Unit = {
      val seen = new IdentityHashMap[G,Long]()
      def go(g: G): Unit = {
        if (isReference(g)) {
          val r = g.asInstanceOf[R]
          val pos = seen.get(g)
          if (pos eq null) {
            seen.put(g, buf.position)
            buf.putByte(RefMarker)
            if (includeRefMetadata) {
              buf.putByte(RefMetadata)
              buf.putFramed(bytePrefix(r))
            }
            else buf.putByte(RefNoMetadata)
            go(dereference(r))
          }
          else {
            buf.putByte(RefSeenMarker.toByte)
            buf.putLong(pos)
          }
        }
        else {
          val pos = seen.get(g)
          if (pos eq null) {
            seen.put(g, buf.position)
            buf.putByte(NestedStartMarker) // indicates a Nested follows
            buf.putFramed(bytePrefix(g))
            foreach(g)(go)
            buf.putByte(NestedEndMarker)
          }
          else {
            buf.putByte(SeenMarker)
            buf.putLong(pos)
          }
        }
      }
      go(_)
    }

    def decode(src: Source): G = {
      case object NestedEnd extends Throwable { override def fillInStackTrace = this }
      var decoded = LongMap.empty[G]

      def read1: G = { val pos = src.position; (src.getByte.toInt: @annotation.switch) match {
        case NestedStartMarker =>
          val g = nested(src.get(src.getInt), readN(Sequence.empty))
          decoded = decoded.updated(pos, g)
          g
        case NestedEndMarker => throw NestedEnd
        case SeenMarker => decoded(src.getLong)
        case RefMarker =>
          val r =
            if (src.getByte.toInt == RefMetadata)
              reference(src.position, src.get(src.getInt))
            else
              reference(src.position, Array.empty)
          decoded = decoded.updated(pos, r)
          val g = read1
          setReference(r, g)
          g
        case RefSeenMarker => decoded(src.getLong)
      }}

      def readN(buf0: Sequence[G]): Sequence[G] = {
        var buf = buf0; while (true) {
          try { buf = buf :+ read1 }
          catch { case NestedEnd => return buf }
        }
        buf
      }
      read1
    }
  }

  final val NestedStartMarker = 0
  final val NestedEndMarker = 1
  final val SeenMarker = 2
  final val RefMarker = 3
  final val RefSeenMarker = 4
  final val RefMetadata = 0
  final val RefNoMetadata = 1
}

