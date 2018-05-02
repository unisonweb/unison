package org.unisonweb.util

import java.nio.ByteBuffer

import org.unisonweb.util.Text.Text

/**
 * A source of bytes which can only be read in a streaming fashion.
 * There is no backtracking or peeking; each read advances the cursor.
 * The cursor position can be accessed via the `position` method.
 */
trait Source { self =>
  def get(n: Int): Array[Byte]
  def getByte: Byte
  def getInt: Int
  def getLong: Long
  def getDouble: Double
  def position: Long
  def getFramed: Array[Byte] = get(getInt)
  final def getString: String = ???
  final def getText: Text = ???

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

