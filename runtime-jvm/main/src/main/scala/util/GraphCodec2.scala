package org.unisonweb.util

import GraphCodec2._

trait GraphCodec2[G] {
  def encode(sink: Sink, seen: G => Option[Long]): G => Unit
  def decode(src: Source, seen: Long => Option[G], done: (Position, G) => Unit): () => G

  def encodeTree(sink: Sink): G => Unit =
    encode(sink, _ => None)

  def decodeTree(src: Source): () => G =
    decode(src, _ => None, (_,_) => ())

  def encodeGraph(sink: Sink): G => Unit = {
    val seen = new java.util.IdentityHashMap[G,Long]
    encode(sink, g => if (seen.containsKey(g)) Some(seen.get(g))
                      else { seen.put(g, sink.position); None })
  }

  def decodeGraph(src: Source): () => G = {
    val seen = new scala.collection.mutable.LongMap[G]()
    decode(src, seen.get, (pos,g) => seen += (pos, g))
  }
}

object GraphCodec2 {
  type Position = Long
}

