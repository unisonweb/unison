package org.unisonweb.util

import java.nio.{ByteBuffer,BufferOverflowException}

object Codecs {

  /**
   * A source of bytes which can only be read in a streaming fashion.
   * There is no backtracking or peeking; each read advances the cursor.
   * The cursor position can be accessed via the `position` method.
   */
  trait Source {
    def get(n: Int): Array[Byte]
    def getByte: Byte
    def getInt: Int
    def getLong: Long
    def getDouble: Double
    def position: Long
    def getFramed: Array[Byte] = get(getInt)
  }

  object Source {
    def apply(bb: ByteBuffer): Source = new Source {
      bb.order(java.nio.ByteOrder.BIG_ENDIAN)

      def get(n: Int) = {
        val arr = new Array[Byte](n)
        bb.get(arr)
        arr
      }

      def getByte: Byte = bb.get
      def getInt: Int = bb.getInt
      def getLong: Long = bb.getLong
      def getDouble: Double = bb.getDouble
      def position: Long = bb.position().toLong
    }
  }

  /**
   * A sink for bytes. Each write appends to the end of the output and
   * advances the cursor position, accessible via the `position` method.
   */
  trait Sink {
    def put(bs: Array[Byte]): Unit
    def putByte(b: Byte): Unit
    def putInt(n: Int): Unit
    def putLong(n: Long): Unit
    def putDouble(n: Double): Unit
    def position: Long
    def putFramed(bs: Array[Byte]): Unit = {
      putInt(bs.length)
      put(bs)
    }
  }

  object Sink {

    def fromByteBuffer(bb: ByteBuffer, onFill: ByteBuffer => Unit): Sink = new Sink {
      var pos: Long = 0L
      def position = pos + bb.position().toLong

      bb.order(java.nio.ByteOrder.BIG_ENDIAN)

      private final def fill = { pos += bb.position(); bb.position(0); onFill(bb) }

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

  def readLong(bs: Array[Byte]): Long = {
    var result = 0L
    val N = java.lang.Long.BYTES
    var i = 0; while (i < N) {
      result <<= 8
      result |= (bs(i) & 0xFF)
      i += 1
    }
    result
  }
}
