package org.unisonweb.util

object Bytes {

  def empty: Sequence[Byte] =
    Sequence.Flat(Deque.fromBuffer(Buffer.viewArray(new Array[Byte](16)), 0))

  def single(b: Byte): Sequence[Byte] =
    Sequence.Flat(Deque.fromBuffer(Buffer.viewArray(Array(b)), 1))

  def apply(bs: Byte*): Sequence[Byte] =
    if (bs.isEmpty) empty
    else Sequence.Flat(Deque.fromBuffer(Buffer.fromArray(bs.toArray), bs.size))

  def viewArray(arr: Array[Byte]): Sequence[Byte] =
    Sequence.Flat(Deque.fromBuffer(Buffer.viewArray(arr), 0))

  def fromArray(arr: Array[Byte]): Sequence[Byte] =
    viewArray(arr.clone)

  class Canonical(val get: Seq.Base, children: Array[Canonical]) {
    def apply(b: Byte) = {
      val bi = b.toInt + 128
      var child = children(bi)
      if (child eq null) {
        child = new Canonical(get :+ (b, this), new Array[Canonical](256))
        children(bi) = child
      }
      child
    }
    def canonicalize(bs: Array[Byte]): Seq.Base = {
      var cur = this
      var i = 0
      while (i < bs.length) {
        cur = cur(bs(i))
        i += 1
      }
      cur.get
    }
  }

  private def mkCanonical = new Canonical(Seq.emptyBase, new Array[Canonical](256))
  private var ref = new java.lang.ref.WeakReference(mkCanonical)

  def Canonical = {
    val r = ref.get
    if (r eq null) {
      val c = mkCanonical
      ref = new java.lang.ref.WeakReference(c)
      c
    }
    else r
  }

  def unsigned(b: Byte): Int = b & 0xff

  case class Seq(bytes: Seq.Base, c: Canonical) {

    /** Lexicographical minimum, assuming unsigned bytes. */
    def min(b2: Seq): Seq = {
      val sdi = this.smallestDifferingIndex(b2)
      if (sdi >= this.size) this
      else if (sdi >= b2.size) b2
      else if (unsigned(this(sdi)) < unsigned(b2(sdi))) this
      else b2
    }

    def size = bytes.size
    def :+(b: Byte) = Seq(bytes :+ (b, c), c)
    def apply(i: Int) = bytes(i)

    def isPrefixOf(s: Seq) =
      try smallestDifferingIndex(s) >= size
      catch { case Seq.NotFound => true } // they are equal

    /** Returns the smallest index such that `this(i) != b(i)`,
      * and throws `NotFound` if the sequences are equal. */
    def smallestDifferingIndex(b: Seq): Int =
      if (c eq b.c) this.bytes.smallestDifferingIndex(b.bytes)
      else {
        var i = 0; var max = size max b.size; while (i < max) {
          try { if (this(i) != b(i)) return i }
          catch { case Seq.OutOfBounds => return i }
          i += 1
        }
        throw Seq.NotFound
      }

    override def equals(a: Any) = {
      val s2 = a.asInstanceOf[Seq]
      if (s2.c eq this.c) this.bytes == s2.bytes
      else (this eq s2) || (size == s2.size && (0 until size).forall(i => bytes(i) == s2.bytes(i)))
    }

    override lazy val hashCode = (0 until size).map(apply(_)).hashCode
  }

  object Seq {

    def empty: Seq = Seq(emptyBase, Canonical)

    def apply(bytes: scala.collection.Seq[Byte]): Seq =
      bytes.foldLeft(empty)(_ :+ _)

    def emptyBase: Base = One(Array())

    case object OutOfBounds extends Throwable
    case object NotFound extends Throwable

    sealed abstract class Base {
      def size: Int
      def :+(b: Byte, c: Canonical): Base
      def apply(i: Int): Byte
      def canonicalize(c: Canonical): Base

      def smallestDifferingIndex(b: Base): Int
    }

    case class One(get: Array[Byte]) extends Base {
      def size = get.length
      def :+(b: Byte, c: Canonical) =
        if (size == 8) Two(c.canonicalize(get), One(Array(b)))
        else One(get :+ b)
      def apply(i: Int) =
        if (i < 0 || i >= get.length) throw OutOfBounds
        else get(i)
      def smallestDifferingIndex(b: Base): Int =
        if (this eq b) throw NotFound
        else b match {
          case Two(b1,b2) if (b1 eq this) => b1.size
          case _ =>
            var i = 0
            while (i < b.size.min(this.size)) {
              if (get(i) != b(i)) return i
              i += 1
            }
            if (this.size == b.size) throw NotFound
            else i
        }

      def canonicalize(c: Canonical) = c.canonicalize(get)
      override def toString = "One(" + get.mkString(", ") + ")"
      override def hashCode = java.util.Arrays.hashCode(get)
      override def equals(a: Any) = a match {
        case a@One(bs2) => (this eq a) || java.util.Arrays.equals(get, bs2)
        case _ => false
      }
    }

    // satisfies invariant that left is always in canonical form and is a complete tree
    case class Two(left: Base, right: Base) extends Base {
      val size = left.size + right.size
      def canonicalize(c: Canonical) = {
        val cright = right.canonicalize(c)
        if (cright eq right) this
        else Two(left, cright)
      }
      def :+(b: Byte, c: Canonical) =
        if (right.size == left.size) Two(this.canonicalize(c), One(Array(b)))
        else Two(left, right :+ (b, c))
      def apply(i: Int) =
        if (i < left.size) left(i)
        else right(i - left.size)

      def smallestDifferingIndex(b: Base): Int =
        if (this eq b) throw NotFound
        else b match {
          case Two(left2,right2) =>
            try left.smallestDifferingIndex(left2)
            catch { case NotFound => left.size + right.smallestDifferingIndex(right2) }
          case One(a) => b.smallestDifferingIndex(this)
        }

      override lazy val hashCode = java.util.Arrays.hashCode(Array(left.hashCode, right.hashCode))

      override def equals(a: Any) = a match {
        case a@Two(left2, right2) => (this eq a) || ((left eq left2) && right == right2)
        case _ => false
      }
    }
  }
}
