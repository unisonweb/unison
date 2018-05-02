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
  def putDouble(n: Double): Unit
  def putString(s: String): Unit
  def putText(txt: Text): Unit
  def position: Long
  def putFramed(bs: Array[Byte]): Unit = {
    putInt(bs.length)
    put(bs)
  }
  def putFramedSeq[A](seq: Seq[A])(f: (Sink,A) => Unit): Unit = {
    putInt(seq.size)
    seq.foreach(a => f(this, a))
  }
}

object Sink {

  def fromByteBuffer(bb: ByteBuffer, onFill: ByteBuffer => Unit): Sink = new Sink {
    var pos: Long = 0L
    def position = pos + bb.position().toLong

    bb.order(java.nio.ByteOrder.BIG_ENDIAN)

    private final def fill = { pos += bb.position(); bb.position(0); onFill(bb) }

    def putString(s: String) =
      // todo: can we do this without allocating a byte buffer?
      // seems like it should be possible
      try {
        val bytes = java.nio.charset.StandardCharsets.UTF_8.encode(s);
        bb.putLong(bytes.position().toLong)
        bb.put(bytes.position(0))
        ()
      }
      catch { case e: BufferOverflowException => fill; putString(s) }

    def putText(txt: Text) =
      // todo: more direct implementation
      putString(Text.toString(txt))

    def put(bs: Array[Byte]) =
      try { bb.put(bs); () }
      catch { case e: BufferOverflowException => fill; bb.put(bs); () }

    def putByte(b: Byte) =
      try { bb.put(b); () }
      catch { case e: BufferOverflowException => fill; bb.put(b); () }

    def putInt(n: Int) =
      try { bb.putInt(n); () }
      catch { case e: BufferOverflowException => fill; bb.putInt(n); () }

    def putLong(n: Long) =
      try { bb.putLong(n); () }
      catch { case e: BufferOverflowException => fill; bb.putLong(n); () }

    def putDouble(n: Double) =
      try { bb.putDouble(n); () }
      catch { case e: BufferOverflowException => fill; bb.putDouble(n); () }
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


