package org.unisonweb.util

sealed abstract class Sequence[A] {
  def apply(i: Long): A
  def ++(s: Sequence[A]): Sequence[A]
  def :+(a: A): Sequence[A]
  def +:(a: A): Sequence[A]
  def size: Long
  def uncons: Option[(A, Sequence[A])]
  def unsnoc: Option[(Sequence[A], A)]
  def headOption: Option[A] =
    if (isEmpty) None
    else Some(apply(0))

  // override def toString = "Sequence(" + (0L until size).map(apply(_)).mkString(", ") + ")"

  override lazy val hashCode = (0L until size).map(apply(_)).hashCode
  override def equals(other: Any) = {
    val s2 = other.asInstanceOf[Sequence[A]]
    size == s2.size && (0L until size).forall(i => apply(i) == s2(i))
  }

  def foldLeft[B](z: B)(f: (B,A) => B): B
  def foreach(f: A => Unit) = foldLeft(())((_,a) => f(a))

  final def isEmpty = size == 0L

  private[util]
  def toDeque: Deque[A]

  def toList = (0L until size).map(apply(_)).toList

  def nest: Sequence[A]

  def take(n: Long): Sequence[A]
  def drop(n: Long): Sequence[A]
  def halve = (take(size / 2), drop(size / 2))
  def reverse: Sequence[A]
  def map[B](f: A => B): Sequence[B]
}

object Sequence {

  val BufferSize = 128

  case class Flat[A](elems: Deque[A]) extends Sequence[A] {
    def apply(i: Long) = elems(i.toInt)
    def ++(s: Sequence[A]): Sequence[A] =
      if (s.size < BufferSize) Flat(elems ++ s.toDeque)
      else s match {
        case Flat(es2) => Nested(elems, Flat(Deque.empty), es2)
        case Nested(left, middle, right) =>
          if (elems.size < BufferSize) Nested(elems ++ left, middle, right)
          else if (left.isEmpty) Nested(elems, middle, right)
          else Nested(elems, left +: middle, right)
      }

    def :+(a: A): Sequence[A] =
      try Flat(elems :+ a)
      catch { case Deque.Overflow => Nested(elems, Flat(Deque.empty), Deque.single(a)) }

    def +:(a: A): Sequence[A] =
      try Flat(a +: elems)
      catch { case Deque.Overflow => Nested(Deque.single(a), Flat(Deque.empty), elems) }

    def size: Long = elems.size
    def toDeque = elems
    def take(n: Long) = Flat(elems.take(n.toInt))
    def drop(n: Long) = Flat(elems.drop(n.toInt))
    def uncons =
      if (elems.isEmpty) None
      else Some((elems(0), Flat(elems.drop(1))))
    def unsnoc =
      if (elems.isEmpty) None
      else Some((Flat(elems.take(elems.size - 1)), elems(elems.size - 1)))
    def reverse = Flat(elems.reverse)
    def map[B](f: A => B) = Flat(elems map f)
    def foldLeft[B](z: B)(f: (B,A) => B) = elems.foldLeft(z)(f)
    def nest = this
  }

  /**
   * Nested sequence with a buffer on each side. Invariant:
   *   * No `Deque` is added to `middle` unless its size >= BufferSize.
   */
  case class Nested[A](left: Deque[A], middle: Sequence[Deque[A]], right: Deque[A]) extends Sequence[A] {
    def :+(a: A): Sequence[A] =
      try Nested(left, middle, right :+ a)
      catch { case Deque.Overflow => Nested(left, middle :+ right, Deque.single(a)) }

    def +:(a: A): Sequence[A] =
      try Nested(a +: left, middle, right)
      catch { case Deque.Overflow => Nested(Deque.single(a), left +: middle, right) }

    def uncons =
      if (left.size > 0) Some(left(0) -> Nested(left drop 1, middle, right))
      else middle.uncons match {
        case None => Flat(right).uncons
        case Some((hd,middle)) => Some(hd(0) -> Nested(hd drop 1, middle, right))
      }

    def unsnoc =
      if (right.size > 0) Some(Nested(left, middle, right take (right.size - 1)) -> right(right.size - 1))
      else middle.unsnoc match {
        case None => Flat(left).unsnoc
        case Some((middle,right)) => Some(Nested(left, middle, right take (right.size - 1)) -> right(right.size - 1))
      }

    def ++(s: Sequence[A]): Sequence[A] =
      if (s.size < BufferSize) Nested(left, middle, right ++ s.toDeque)
      else s match {
        case Flat(es2) =>
          if (right.size < BufferSize) Nested(left, middle, right ++ es2)
          else Nested(left, middle :+ (right ++ es2.take(BufferSize)), es2.drop(BufferSize)).nest
        case Nested(left2, middle2, right2) => {
          if (left2.size < BufferSize) Nested(left, (middle :+ (right ++ left2)) ++ middle2, right2)
          else Nested(left, (middle :+ right :+ left2) ++ middle2, right2)
        }.nest
      }

    lazy val prefixSizes = {
      var buf = new Array[Long](middle.size.toInt)
      var acc = 0L
      var i = 0
      while (i < buf.length) { acc = acc + middle(i).size; buf(i) = acc; i += 1 }
      buf
    }

    lazy val middleSize = if (middle.isEmpty) 0L else prefixSizes(prefixSizes.size - 1)

    lazy val size = left.size + middleSize + right.size

    def apply(i: Long) =
      if (i < left.size) left(i.toInt)
      else if (i >= left.size + middleSize) right((i - left.size - middleSize).toInt)
      else { // i < left.size + middleSize
        val j = i - left.size
        // tricky
        // [[a,b], [c,d,e], [f,g,h,i]] // middles
        // [2,     5,       9        ] // middleSizes
        val k = Sequence.lubIndex(j + 1, prefixSizes)
        if (k > 0) {
          val rem = (j - prefixSizes(k - 1)).toInt
          middle(k)(rem)
        }
        else middle(k)(j.toInt)
      }

    def take(n: Long) =
      if (n <= left.size) Flat(left take n.toInt)
      else if (n >= left.size + middleSize) Nested(left, middle, right take (n - (left.size + middleSize)).toInt)
      else {
        val j = n - left.size
        val k = Sequence.lubIndex(j, prefixSizes)
        if (k > 0) {
          val rem = (j - prefixSizes(k - 1)).toInt
          (middle.take(k) :+ middle(k).take(rem)).unsnoc match {
            case Some((middle,right)) => Nested(left,middle,right)
            case None => Flat(left)
          }
        }
        else Flat(left) ++ Flat(middle(0).take(j.toInt))
      }

    def drop(n: Long) =
      if (n <= left.size) Nested(left.drop(n.toInt), middle, right)
      else if (n >= left.size + middleSize) Flat(right drop (n - (left.size + middleSize)).toInt)
      else {
        val j = n - left.size
        val k = Sequence.lubIndex(j, prefixSizes)
        if (prefixSizes(k) == j) middle.drop(k+1).uncons match {
          case Some((left,middle)) => Nested(left, middle, right)
          case None => Flat(right)
        }
        else if (k == 0) Nested(middle(k).drop(j.toInt), middle.drop(k+1), right)
        else {
          val rem = (j - prefixSizes(k - 1)).toInt
          Nested(middle(k).drop(rem),  middle.drop(k+1), right)
        }
      }

    def reverse = Nested(right.reverse, middle.map(_.reverse).reverse, left.reverse) // nice!

    def map[B](f: A => B) = Nested(left map f, middle map (_ map f), right map f)

    def toDeque =
      middle.foldLeft(left)(_ ++ _) ++ right

    def foldLeft[B](z: B)(f: (B,A) => B) = {
      val leftB = left.foldLeft(z)(f)
      val middleB = middle.foldLeft(leftB)((b,nestf) => nestf.foldLeft(b)(f))
      val rightB = right.foldLeft(middleB)(f)
      rightB
    }

    def nest =
      if (middle.size > BufferSize) middle.halve match {
        case (midL, midR) => Nested(left, Nested(midL.toDeque, Flat(Deque.empty), midR.toDeque), right)
      }
      else this
  }

  def single[A](a: A): Sequence[A] =
    Flat(Deque.single(a))

  def empty[A]: Sequence[A] =
    Flat(Deque.empty)

  def apply[A](as: A*): Sequence[A] =
    Flat(as.foldLeft(Deque.empty[A])((buf,a) => buf :+ a))

  /** Assuming `vs` is sorted in increasing order, find the index of the first value >= `v`. */
  def lubIndex(target: Long, vs: Array[Long]): Int = {
    // todo - tune this, possibly use skewed search
    var low = 0
    var high = vs.length - 1
    while (low <= high) {
      var mid = (low + high) >>> 1
      val midv = vs(mid)
      if (midv < target) low = mid + 1
      else if (midv > target) high = mid - 1
      else { while (mid > 0 && vs(mid - 1) == midv) { mid -= 1 }; return mid }
    }
    low
  }
}
