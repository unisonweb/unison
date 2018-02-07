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

  private def mkCanonical = new Canonical(Seq.empty, new Array[Canonical](256))
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

  case class Seq(bytes: Seq.Base, c: Canonical) {
    def size = bytes.size
    def :+(b: Byte) = Seq(bytes :+ (b, c), c)
    def apply(i: Int) = bytes(i)
    def smallestDifferingIndex(start: Int, b: Seq): Int =
      this.bytes.smallestDifferingIndex(start, b.bytes)
  }

  object Seq {
    def empty: Base = One(Array())

    sealed abstract class Base {
      def size: Int
      def :+(b: Byte, c: Canonical): Base
      def apply(i: Int): Byte
      def canonicalize(c: Canonical): Base
      def equal(b: Base): Boolean = {
        //todo - more efficient impl
        (this eq b) || (size == b.size && (0 until size).forall(i => this(i) == b(i)))
      }
      /** Returns the smallest index, `i >= start`, such that `this(i) != b(i)`,
       *  and -1 if all indices >= start are the same. */
      def smallestDifferingIndex(start: Int, b: Base): Int = {
        // todo - more efficient impl
        if (size != b.size) (size min b.size)
        else {
          var i = start
          while (i < size) { if (this(i) != b(i)) return i; i += 1 }
          -1
        }
      }
    }

    case class One(get: Array[Byte]) extends Base {
      def size = get.length
      def :+(b: Byte, c: Canonical) =
        if (size == 8) Two(c.canonicalize(get), One(Array(b)))
        else One(get :+ b)
      def apply(i: Int) = get(i)
      def canonicalize(c: Canonical) = c.canonicalize(get)
      override def toString = "One(" + get.mkString(", ") + ")"
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
    }
  }
}
