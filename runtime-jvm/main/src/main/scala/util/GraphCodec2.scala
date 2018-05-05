package org.unisonweb.util

import GraphCodec2._

trait GraphCodec2[G] {
  def encode(sink: Sink, seen: G => Option[Long]): G => Unit
  def decode(src: Source, seen: Long => Option[G], done: G => G with Done): () => G with Done

  def encodeTree(sink: Sink): G => Unit =
    encode(sink, _ => None)

  def decodeTree(src: Source): () => G =
    decode(src, _ => None, g => g.asInstanceOf[G with Done])

  def encodeGraph(sink: Sink): G => Unit = {
    val seen = new java.util.IdentityHashMap[G,Long]
    encode(sink, g => if (seen.containsKey(g)) Some(seen.get(g))
                      else { seen.put(g, sink.position); None })
  }

  def decodeGraph(src: Source): () => G = {
    val seen = new scala.collection.mutable.LongMap[G]()
    decode(src, seen.get, g => { seen += (src.position, g); g.asInstanceOf[G with Done]})
  }
}

object GraphCodec2 {
  trait Done
}

