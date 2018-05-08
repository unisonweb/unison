package org.unisonweb.util

import GraphCodec._

trait GraphCodec[G] {

  // type K
  // def objectIdentity(g: G): K

  def objectIdentity(g: G): Any

  def encode(sink: Sink, seen: G => Option[Position]): G => Unit

  def decode(src: Source, seen: Position => Option[G], done: (Position, G) => Unit): () => G

  def encodeTree(sink: Sink): G => Unit =
    encode(sink, _ => None)

  def decodeTree(src: Source): () => G =
    decode(src, _ => None, (_,_) => ())

  def encodeGraph(sink: Sink): G => Unit = {
    val seen = new java.util.IdentityHashMap[Any,Long]
    encode(sink, g => {
      val id = objectIdentity(g)
      if (seen.containsKey(id)) Some(seen.get(id))
      else { seen.put(id, sink.position); None }
    })
  }

  def decodeGraph(src: Source): () => G = {
    val seen = new scala.collection.mutable.LongMap[G]()
    decode(src, seen.get, (pos,g) => seen += (pos, g))
  }

  /** Convenience function to write out a sequence of byte chunks for a `G`. */
  def encode(g: G): Sequence[Array[Byte]] = {
    var buf = Sequence.empty[Array[Byte]]
    val bb = java.nio.ByteBuffer.allocate(1024)
    val encoder = encodeGraph(Sink.fromByteBuffer(bb, arr => buf = buf :+ arr))
    encoder(g)
    if (bb.position() != 0) {
      // there are leftover bytes buffered in `bb`, flush them
      val rem = new Array[Byte](bb.position)
      bb.position(0)
      bb.get(rem)
      buf :+ rem
    }
    else buf
  }

  /** Convenience function to read a `G` from a sequence of chunks. */
  def decode(chunks: Sequence[Array[Byte]]): G =
    decodeGraph(Source.fromChunks(chunks))()
}

object GraphCodec {
  type Position = Long
}

