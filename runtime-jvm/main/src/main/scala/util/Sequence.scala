package org.unisonweb.util

abstract class Sequence[A] {
  import Sequence._
  def apply(i: Long): A
  def ++(s: Sequence[A]): Sequence[A]
  def :+(a: A): Sequence[A]
  def +:(a: A): Sequence[A]
  def size: Long
  def uncons: Option[(A, Sequence[A])]
  def unsnoc: Option[(Sequence[A], A)]

  override def toString = "Sequence(" + (0L until size).map(apply(_)).mkString(", ") + ")"

  override lazy val hashCode = (0L until size).map(apply(_)).hashCode
  override def equals(other: Any) = {
    val s2 = other.asInstanceOf[Sequence[A]]
    size == s2.size && (0L until size).forall(i => apply(i) == s2(i))
  }

  def foldLeft[B](z: B)(f: (B,A) => B): B =
    (0L until size).foldLeft(z)((b,i) => f(b, apply(i)))

  final def isEmpty = size == 0L

  private[util]
  def toDeque: Deque[A]

  def nest: Sequence[A] = this match {
    case Nested(left, middle, right) if middle.size > BufferSize => middle.halve match {
      case (midL, midR) => Nested(left, Flat(midL.toDeque) ++ Flat(midR.toDeque), right)
    }
    case _ => this
  }

  def take(n: Long): Sequence[A]
  def drop(n: Long): Sequence[A]
  def halve = (take(size / 2), drop(size / 2))
}

object Sequence {

  val BufferSize = 2

  case class Flat[A](elems: Deque[A]) extends Sequence[A] {
    def apply(i: Long) = elems(i.toInt)
    def ++(s: Sequence[A]): Sequence[A] =
      if (s.size + size < BufferSize * 2) Flat(elems ++ s.toDeque)
      else s match {
        case Flat(es2) => Nested(elems, Flat(Deque.empty), es2)
        case Nested(left, middle, right) =>
          if (elems.size < BufferSize) Nested(elems ++ left, middle, right)
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

    def take(n: Long) =
      if (n <= left.size) Flat(left take n.toInt)
      else if (n > left.size + middleSize) Flat(right take (n - (left.size + middleSize)).toInt)
      else middle.uncons match {
        case Some((mh, mt)) => Nested(mh, mt, right).take(n - left.size)
        case None => Flat(right).take(n - left.size)
      }

    def drop(n: Long) =
      if (n < left.size) Nested(left.drop(n.toInt), middle, right)
      else if (n > left.size + middleSize) Flat(right drop (n - (left.size + middleSize)).toInt)
      else middle.uncons match {
        case Some((mh, mt)) => Nested(mh, mt, right).drop(n - left.size)
        case None => Flat(right).drop(n - left.size)
      }

    def ++(s: Sequence[A]): Sequence[A] =
      if (s.size < BufferSize) Nested(left, middle, right ++ s.toDeque)
      else s match {
        case Flat(es2) =>
          if (right.size < BufferSize) Nested(left, middle, right ++ es2)
          else Nested(left, middle :+ (right ++ es2.take(BufferSize)), es2.drop(BufferSize)).nest
        case Nested(left2, middle2, right2) => {
          if (left2.size < BufferSize) {
            middle2.foldLeft(Nested(left, middle, right ++ left2): Sequence[A]) { (buf, chunk) =>
              buf ++ Flat(chunk)
            } ++ Flat(right)
          }
          else Nested(left, (middle :+ left2) ++ middle2, right2)
        }.nest
      }

    def apply(i: Long) =
      if (i < left.size) left(i.toInt)
      else if (i >= left.size + middleSize) right((i - left.size - middleSize).toInt)
      else {
        var j = i - left.size
        var k = 0
        while (j >= middle(k).size) { j -= middle(k).size; k += 1 }
        middle(k)(j.toInt)
      }

    def toDeque = sys.error("nested with size < BufferSize: " + this)

    lazy val middleSize = {
      var sz = 0L
      (0L until middle.size).foreach { i => sz += middle(i).size }
      sz
    }

    lazy val size = left.size + middleSize + right.size
  }

  def single[A](a: A): Sequence[A] =
    Flat(Deque.single(a))

  def empty[A]: Sequence[A] =
    Flat(Deque.empty)

  def apply[A](as: A*): Sequence[A] =
    Flat(as.foldLeft(Deque.empty[A])((buf,a) => buf :+ a))

}

