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
  def putVarLong(n: Long): Unit = putLong(n)
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
    putInt(seq.size)
    seq.foreach(f)
  }
  def putOption1[A](o: Option[A])(f: A => Unit): Unit = o match {
    case None => putByte(0)
    case Some(a) => putByte(1); f(a)
  }
}

object Sink {

  def fromByteBuffer(bb: ByteBuffer, onFill: Array[Byte] => Unit): Sink = new Sink {
    var pos: Long = 0L
    def position = pos + bb.position().toLong

    bb.order(java.nio.ByteOrder.BIG_ENDIAN)

    private final def fill = {
      val buf = new Array[Byte](bb.position())
      pos += buf.length
      bb.position(0)
      bb.get(buf)
      onFill(buf)
      bb.position(0)
    }

    def putString(s: String) =
      // todo: can we do this without allocating a byte buffer?
      // seems like it should be possible
      try putFramed(s.getBytes(java.nio.charset.StandardCharsets.UTF_8))
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


