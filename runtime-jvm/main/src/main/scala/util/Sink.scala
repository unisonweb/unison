package org.unisonweb.util

import java.nio.{ByteBuffer,BufferOverflowException}
import Text.Text

/**
 * A sink for bytes. Each write appends to the end of the output and
 * advances the cursor position, accessible via the `position` method.
 */
trait Sink {
  def put(bs: Array[Byte]): Unit
  def putBoolean(b: Boolean): Unit = if (b) putByte(1) else putByte(0)
  def putByte(b: Byte): Unit
  def putInt(n: Int): Unit
  def putLong(n: Long): Unit

  // todo: the UTF-8 of Long encoding, use a single byte if possible
  def putVarLong(n: Long): Unit =
    putLong(n)
  def putDouble(n: Double): Unit
  def putString(s: String): Unit
  def putText(txt: Text): Unit
  def position: Long
  def putFramed(bs: Array[Byte]): Unit = {
    putInt(bs.length)
    put(bs)
  }
  def putFramedSeq[A](seq: Seq[A])(f: (Sink,A) => Unit): Unit =
    putFramedSeq1(seq)(a => f(this, a))
  def putFramedSeq1[A](seq: Seq[A])(f: A => Unit): Unit = {
    putVarLong(seq.size.toLong)
    seq.foreach(f)
  }
  def putOption1[A](o: Option[A])(f: A => Unit): Unit = o match {
    case None => putByte(0)
    case Some(a) => putByte(1); f(a)
  }
}

object Sink {

  def toChunks(bufferSize: Int)(f: Sink => Unit): Sequence[Array[Byte]] = {
    var buf = Sequence.empty[Array[Byte]]
    val bb = java.nio.ByteBuffer.allocate(bufferSize)
    f(Sink.fromByteBuffer(bb, arr => buf = buf :+ arr))
    if (bb.position() != 0) {
      bb.flip()
      // there are leftover bytes buffered in `bb`, flush them
      val rem = new Array[Byte](bb.limit())
      bb.get(rem)
      buf :+ rem
    }
    else buf
  }

  def fromByteBuffer(bb: ByteBuffer, onFill: Array[Byte] => Unit): Sink = new Sink {
    var pos: Long = 0L
    def position = pos + bb.position().toLong

    bb.order(java.nio.ByteOrder.BIG_ENDIAN)

    private final def empty = {
      bb.flip() // reset position back to 0, set limit to position
      val buf = new Array[Byte](bb.limit())
      pos += buf.length
      bb.get(buf) // this fills the array
      onFill(buf)
      bb.clear()
    }

    def putString(s: String) =
      // todo: can we do this without allocating a byte buffer?
      // seems like it should be possible
      putFramed(s.getBytes(java.nio.charset.StandardCharsets.UTF_8))

    def putText(txt: Text) =
      // todo: more direct implementation
      putString(Text.toString(txt))

    def put(bs: Array[Byte]) =
      if (bs.length < bb.capacity())
        try { bb.put(bs); () }
        catch { case e: BufferOverflowException => empty; bb.put(bs); () }
      else bs.splitAt(bs.length / 2) match {
        case (bs1, bs2) => put(bs1); put(bs2)
      }

    def putByte(b: Byte) =
      try { bb.put(b); () }
      catch { case e: BufferOverflowException => empty; bb.put(b); () }

    def putInt(n: Int) =
      try { bb.putInt(n); () }
      catch { case e: BufferOverflowException => empty; bb.putInt(n); () }

    def putLong(n: Long) =
      try { bb.putLong(n); () }
      catch { case e: BufferOverflowException => empty; bb.putLong(n); () }

    def putDouble(n: Double) =
      try { bb.putDouble(n); () }
      catch { case e: BufferOverflowException => empty; bb.putDouble(n); () }
  }

  def writeLong(n: Long): Array[Byte] = {
    val N = java.lang.Long.BYTES
    val result = new Array[Byte](N)
    var m = n
    var i = N - 1; while (i >= 0) {
      result(i) = (m & 0xFF).toByte
      m >>= 8
      i -= 1
    }
    return result;
  }
}


