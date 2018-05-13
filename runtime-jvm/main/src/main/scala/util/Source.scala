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
  // todo: The UTF-8 of Long encodings, uses a single byte where possible
  def getVarLong: Long = getLong
  def getDouble: Double
  def position: Long
  def getFramed: Array[Byte] = get(getInt)

  final def getString: String = {
    val bytes = getFramed
    new String(bytes, java.nio.charset.StandardCharsets.UTF_8)
  }

  final def getText: Text = Text.fromString(getString)

  def getOption1[A](a: => A): Option[A] =
    if (getByte == 0) None
    else Some(a)

  def getOption[A](a: Source => A): Option[A] =
    getOption1(a(this))

  def getFramedArray1[A:reflect.ClassTag](a: => A): Array[A] =
    Array.fill(getVarLong.toInt)(a)

  def getFramedArray[A:reflect.ClassTag](a: Source => A): Array[A] =
    getFramedArray1(a(this))

  def getFramedList1[A](a: => A): List[A] =
    List.fill(getVarLong.toInt)(a)

  def getFramedList[A](f: Source => A): List[A] =
    getFramedList1(f(this))

  def getFramedSequence1[A](a: => A): Sequence[A] =
    Sequence.fill(getVarLong)(a)

  @annotation.tailrec
  final def foreachDelimited[A](decode1: => A)(each: A => Unit): Unit =
    getByte match {
      case 0 => ()
      case 111 =>
        each(decode1)
        foreachDelimited(decode1)(each)
      case b => sys.error("unknown byte in foreachDelimited: " + b)
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

  def fromChunks(bufferSize: Int)(chunks: Sequence[Array[Byte]]): Source = {
    val bb = java.nio.ByteBuffer.allocate(bufferSize)
    var rem = chunks
    bb.limit(0)
    Source.fromByteBuffer(bb, (unread, bb) => rem.uncons match {
      case None => throw Underflow()
      case Some((chunk,chunks)) =>
        bb.put(unread)
        if (chunk.length <= bb.remaining()) {
          bb.put(chunk)
          rem = chunks
        }
        else { // need to split up chunk
          val (c1,c2) = chunk.splitAt(bb.remaining())
          bb.put(c1)
          rem = c2 +: chunks
        }
    })
  }

  object BufferUnderflow {
    import java.nio.BufferUnderflowException
    def unapply(e: Throwable): Boolean = e match {
      case e : BufferUnderflowException => true
      case i : ArrayIndexOutOfBoundsException => true
      case _ => false
    }
  }

  def fromByteBuffer(bb: ByteBuffer, onEmpty: (Array[Byte], ByteBuffer) => Unit): Source = new Source {
    bb.order(java.nio.ByteOrder.BIG_ENDIAN)
    var pos = 0L

    def position: Long = pos + bb.position().toLong

    def refill = {
      pos += bb.position()
      val unread =
        if (bb.remaining() > 0) {
          val unread = new Array[Byte](bb.remaining())
          bb.put(unread)
          unread
        }
        else Array.empty[Byte]
      bb.clear()
      onEmpty(unread, bb)
      bb.flip()
    }

    def get(n: Int) =
      try {
        val arr = new Array[Byte](n)
        bb.get(arr)
        arr
      }
      catch { case BufferUnderflow() =>
        if (n <= bb.capacity()) { refill; get(n) }
        else get(n/2) ++ get(n - n/2)
      }

    def getByte: Byte =
      try bb.get
      catch { case BufferUnderflow() => refill; getByte }

    def getInt: Int =
      try bb.getInt
      catch { case BufferUnderflow() => refill; getInt }

    def getLong: Long =
      try bb.getLong
      catch { case BufferUnderflow() => refill; getLong }

    def getDouble: Double =
      try bb.getDouble
      catch { case BufferUnderflow() => refill; getDouble }
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

