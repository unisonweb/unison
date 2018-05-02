package org.unisonweb.util

import java.nio.ByteBuffer

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

