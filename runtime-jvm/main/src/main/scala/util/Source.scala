package org.unisonweb.util

import java.nio.ByteBuffer

import org.unisonweb.util.Text.Text

import scala.reflect.ClassTag

/**
 * A source of bytes which can only be read in a streaming fashion.
 * There is no backtracking or peeking; each read advances the cursor.
 * The cursor position can be accessed via the `position` method.
 */
trait Source { self =>
  def get(n: Int): Array[Byte]
  def getBoolean: Boolean = getByte != 0
  def getByte: Byte
  def getInt: Int
  def getLong: Long
  def getDouble: Double
  def position: Long
  def getFramed: Array[Byte] = get(getInt)

  final def getString: String = {
    val bytes = getFramed
    new String(bytes, java.nio.charset.StandardCharsets.UTF_8)
  }
  final def getText: Text = Text.fromString(getString)

  /** Checks `ok` before each operation, throws `Source.Invalidated` if `!ok`. */
  def invalidateWhen(invalidated: => Boolean): Source = new Source {
    def position =
      if (!invalidated) self.position
      else throw Source.Invalidated()
    def get(n: Int) =
      if (!invalidated) self.get(n)
      else throw Source.Invalidated()
    def getByte: Byte =
      if (!invalidated) self.getByte
      else throw Source.Invalidated()
    def getInt: Int =
      if (!invalidated) self.getInt
      else throw Source.Invalidated()
    def getLong: Long =
      if (!invalidated) self.getLong
      else throw Source.Invalidated()
    def getDouble: Double =
      if (!invalidated) self.getDouble
      else throw Source.Invalidated()
  }

  def take(m: Long): Source = new Source {
    val end = (self.position + m) max self.position
    def position = self.position
    def remaining = end - self.position
    def get(n: Int) =
      if (remaining >= n) self.get(n)
      else throw Source.Underflow()
    def getByte: Byte =
      if (remaining > 0) self.getByte
      else throw Source.Underflow()
    def getInt: Int =
      if (remaining > 3) self.getInt
      else throw Source.Underflow()
    def getLong: Long =
      if (remaining > 7) self.getLong
      else throw Source.Underflow()
    def getDouble: Double =
      if (remaining > 7) self.getDouble
      else throw Source.Underflow()
  }

}

object Source {

  case class Underflow() extends Throwable
  case class Invalidated() extends Throwable

  def fromChunks(chunks: Sequence[Array[Byte]]): Source = chunks.uncons match {
    case None => sys.error("empty chunks")
    case Some((chunk,chunks)) => {
      val bb = java.nio.ByteBuffer.allocate(chunk.size * 2)
      bb.put(chunk)
      bb.position(0)
      var rem = chunks
      Source.fromByteBuffer(bb, bb => rem.uncons match {
        case None => throw Underflow()
        case Some((chunk,chunks)) =>
          if (bb.limit() >= chunk.length) {
            bb.put(chunk)
            rem = chunks
          }
          else { // need to split up chunk
            val (c1,c2) = chunk.splitAt(bb.limit())
            bb.put(c1)
            rem = c2 +: chunks
          }
      })
    }
  }

  object BufferUnderflow {
    import java.nio.BufferUnderflowException
    def unapply(e: Throwable): Boolean = e match {
      case e : BufferUnderflowException => true
      case i : ArrayIndexOutOfBoundsException => true
      case _ => false
    }
  }

  def fromByteBuffer(bb: ByteBuffer, onEmpty: ByteBuffer => Unit): Source = new Source {
    bb.order(java.nio.ByteOrder.BIG_ENDIAN)
    var pos = 0L

    def position: Long = pos + bb.position().toLong

    def empty = {
      pos += bb.position()
      bb.flip()
      onEmpty(bb)
      bb.flip()
    }

    def get(n: Int) =
      try {
        val arr = new Array[Byte](n)
        bb.get(arr)
        arr
      }
      catch { case BufferUnderflow() => empty; get(n) }

    def getByte: Byte =
      try { val b = bb.get; println("getting a byte: " + b); b }
      catch { case BufferUnderflow() => empty; getByte }

    def getInt: Int =
      try bb.getInt
      catch { case BufferUnderflow() => empty; getInt }

    def getLong: Long =
      try bb.getLong
      catch { case BufferUnderflow() => empty; getLong }

    def getDouble: Double =
      try bb.getDouble
      catch { case BufferUnderflow() => empty; getDouble }

    override def getFramed: Array[Byte] = {
      val originalPos = bb.position()
      try {
        val size = getInt
        val bytes = new Array[Byte](size)
        bb.get(bytes)
        bytes
      }
      catch {
        case BufferUnderflow() =>
          bb.position(originalPos)
          empty
          getFramed
      }
    }
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

  def getFramedArray[A:ClassTag](src: Source)(f: Source => A): Array[A] = {
    val len = src.getInt
    Array.fill(len)(f(src))
  }
  def getFramedList[A](src: Source)(f: Source => A): List[A] = {
    val len = src.getInt
    List.fill(len)(f(src))
  }
}

