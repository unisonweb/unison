package org.unisonweb.util

import java.util.IdentityHashMap
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.immutable.LongMap

/**
 * Encoder/decoder for graphs of type `G` with references of type `R`.
 *
 * Each `G` has some number of children, which are also of type `G`,
 * accessed via `foreach`.
 *
 * Some `G` are _references_, of type `R`, which can be set via `setReference`
 * and created via `makeReference`.
 *
 * Each `G` also has some binary data, called the _byte prefix_, accessed
 * via `writeBytePrefix`, `bytePrefixLength`, `bytePrefixIndex`.
 *
 * The interface requires that the byte prefix plus the children be sufficient
 * to reconstitute the `G`. That is, it satisfies:
 *
 *   nest(bytePrefix(g).toArray, children(g)) == g
 */
trait GraphCodec[G,R<:G] {
  import GraphCodec._

  def writeBytePrefix(graph: G, sink: Sink): Unit

  def bytePrefixLength(graph: G): Int

  def bytePrefix(graph: G): Sequence[Byte] = {
    var buf = Bytes.empty
    val bb = java.nio.ByteBuffer.allocate(128)
    val sink: Sink = Sink.fromByteBuffer(bb,
      bb => buf = buf ++ Bytes.fromArray(bb.array())
    )
    writeBytePrefix(graph, sink)
    val rem = new Array[Byte](bb.position)
    bb.position(0)
    bb.get(rem)
    buf = buf ++ Bytes.viewArray(rem)
    buf
  }

  /** Returns the `index`th byte (0-based) of the byte prefix. */
  def bytePrefixIndex(graph: G, index: Int): Byte

  def foreach(graph: G)(f: G => Unit): Unit

  def children(graph: G): Sequence[G] = {
    var cs = Sequence.empty[G]
    foreach(graph) { g => cs = cs :+ g }
    cs
  }

  /**
   * Create an empty `R` from a position and a `prefix`.
   * It should be subsequently set via `setReference`.
   */
  def makeReference(position: Long, prefix: Array[Byte]): R
  def setReference(ref: R, referent: G): Unit
  def isReference(graph: G): Boolean
  def dereference(graph: R): G

  /**
   * Create a `G` given a byte prefix and sequence of children.
   * Byte prefix will generally identify which constructor it is,
   * and any auxiliary info. Implementations should read from
   * `prefix` BEFORE calling `readChild`.
   */
  def nest(prefix: Source, readChild: () => Option[G]): G

  def foldLeft[B](graph: G)(b0: B)(f: (B,G) => B): B = {
    var b = b0
    foreach(graph)(g => b = f(b,g))
    b
  }

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
            writeBytePrefix(r, buf)
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
          writeBytePrefix(g, buf)
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
        var reachedEnd = new AtomicBoolean(false)
        var invalidated = new AtomicBoolean(false)
        val next = readChild(invalidated, reachedEnd)
        val g = nest(src.invalidateWhen(invalidated.get), next)
        drain(next)
        decoded = decoded.updated(pos, g)
        g
      case NestedEndMarker => throw NestedEnd
      case SeenMarker => decoded(src.getLong)
      case RefMarker =>
        val r =
          if (src.getByte.toInt == RefMetadata)
            makeReference(src.position, src.get(src.getInt))
          else
            makeReference(src.position, Array.empty)
        decoded = decoded.updated(pos, r)
        val g = read1
        setReference(r, g)
        g
      case RefSeenMarker => decoded(src.getLong)
    }}

    @annotation.tailrec
    def drain(f: () => Option[G]): Unit = f() match {
      case None => ()
      case Some(_) => drain(f)
    }

    def readChild(invalidate: AtomicBoolean, reachedEnd: AtomicBoolean): () => Option[G] = () => {
      invalidate.set(true)
      if (!reachedEnd.get) {
        try Some(read1)
        catch { case NestedEnd => reachedEnd.set(true); None }
      }
      else None
    }

    read1
  }

}

object GraphCodec {
  final val NestedStartMarker = 0
  final val NestedEndMarker = 1
  final val SeenMarker = 2
  final val RefMarker = 3
  final val RefSeenMarker = 4
  final val RefMetadata = 0
  final val RefNoMetadata = 1
}

