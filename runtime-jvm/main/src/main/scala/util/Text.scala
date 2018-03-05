package org.unisonweb.util

import java.lang.{Character,StringBuilder}

object Text {

  type Codepoint = Int

  type Text = Sequence[Codepoint]

  /** The empty `Text`, consists of no characters. */
  def empty: Text =
    Sequence.Flat(Deque.fromBuffer(emptyBuffer, 0))

  /** Converts from `String` to `Text`. */
  def fromString(s: String): Text = {
    val buf = emptyBuffer
    var i = 0; var j = 0; while (i < s.length) {
      val ch = s.codePointAt(i)
      buf :+ (j, ch)
      if (Character.charCount(ch) == 2) i += 1
      j += 1
      i += 1
    }
    Sequence.Flat(Deque.fromBuffer(buf, j))
  }

  /** Convert a `Text` back to a `String`. */
  def toString(t: Text): String = {
    val buf = new StringBuilder()
    var i = 0L
    while (i < t.size) { buf.appendCodePoint(t(i)); i += 1L }
    buf.toString
  }

  def emptyBuffer: Buffer[Codepoint] = TBuffer(new StringBuilder(), Array.empty[Int], 0)

  /**
   * A `Buffer[Codepoint]`. Indices into this `Buffer` refer to codepoints,
   * not Java `char` values. The `i`th value of this `Buffer` is a
   * `String` consisting of the `i`th codepoint (which might need 1 or 2
   * `char` values to encode, since the Java `char` type is only 16 bits and
   * 21 bits are needed to express any codepoint in Unicode).
   *
   * NB: There is a huge amount of broken code out there that indexes directly
   * into an `Array[Char]` or `String` and assumes the result is something meaningful,
   * when in fact that code only works if all characters being used fit in the 16 bit
   * `char` type that Java provides.
   */
  private case class TBuffer(buf: StringBuilder,
                             var multicharPositions: Array[Int],
                             var numMultichar: Int) extends Buffer[Codepoint] {

    def codepointSize = buf.length - numMultichar

    override def toString =
      "StringBuilder(" + buf.toString.length + ") | " + multicharPositions.mkString(" ")

    private def charIndex(codepointIndex: Int): Int = {
      val ind0 = java.util.Arrays.binarySearch(multicharPositions, codepointIndex)
      val offset = if (ind0 < 0) (ind0 + 1).abs else ind0
      codepointIndex + offset
    }

    def apply(i: Int) =
      buf.codePointAt(charIndex(i))

    def copy(from: Int, len: Int) = {
      val buf2 = emptyBuffer
      var i = from
      var j = 0
      while (i < from + len) { buf2 :+ (j, apply(i)); j += 1; i += 1 }
      buf2
    }

    def copyTo(destIndex: Int, dest: Buffer[Codepoint], len: Int) = dest match {
      case b@TBuffer(buf2, m2, nm2) if len == codepointSize && destIndex == b.codepointSize =>
        val bsize = b.codepointSize
        b.multicharPositions = b.multicharPositions.take(nm2) ++
                               multicharPositions.take(numMultichar).map(_ + bsize)
        b.numMultichar = b.numMultichar + numMultichar
        buf2.append(buf)
        b
      case dest => dest :++ (destIndex, toArray(len), len)
    }

    private def addMulticharPosition(i: Int) = {
      if (numMultichar >= multicharPositions.length) {
        multicharPositions = multicharPositions ++ Array.fill(multicharPositions.length max 1)(Int.MaxValue)
      }
      multicharPositions(numMultichar) = i
      numMultichar += 1
    }

    def :+(i0: Int, ch: Codepoint) = {
      // val i = charIndex(i0)
      // require (i == buf.length)
      buf.appendCodePoint(ch)
      if (Character.charCount(ch) == 2) addMulticharPosition(i0)
      this
    }

    def :++(i0: Int, str: Array[Codepoint], len: Int) = {
      // val i = charIndex(i0)
      // require(i == buf.length)
      var j = 0; while (j < len && j < str.length) {
        val ch = str(j)
        if (Character.charCount(ch) == 2) addMulticharPosition(i0 + j)
        buf.appendCodePoint(ch)
        j += 1
      }
      this
    }

    def empty = Text.TBuffer(new StringBuilder(), Array.empty, 0)

    def toArray(size: Int): Array[Codepoint] = {
      var rem = size
      var i = 0
      var j = 0
      var out = new Array[Codepoint](rem)
      while (rem > 0) {
        val ch = buf.codePointAt(i)
        out(j) = ch
        if (Character.charCount(ch) == 2) i += 1
        j += 1
        rem -= 1
        i += 1
      }
      out
    }
  }
}
