package org.unisonweb.util

//class SnocSequence[A](val size: Long, hd: Buffer[A], tl: SnocSequence[Buffer[A]], buf: Buffer[A]) {
//
//  private final def tlSize = if (tl eq null) 0 else tl.size * Buffer.Arity
//  private final val hdSizePlusTlSize = hd.size + tlSize
//
//  def apply(i: Long): A = {
//    if (i >= hdSizePlusTlSize) buf(i - hdSizePlusTlSize)
//    else if (i < Buffer.Arity) hd(i)
//    else tl((i - Buffer.Arity) / Buffer.Arity)(i % Buffer.Arity)
//  }
//
//  def :+(a: A): SnocSequence[A] = (buf :+ a) match { case buf =>
//    if (buf.size != Buffer.Arity) new SnocSequence(size + 1, hd, tl, buf)
//    else if (size == Buffer.Arity - 1) new SnocSequence(Buffer.Arity, buf, tl, Buffer.empty[A])
//    else new SnocSequence(size + 1, hd,
//                          if (tl eq null) SnocSequence.single(buf) else tl :+ buf,
//                          Buffer.empty[A])
//  }
//
//  /* just fill up the buffer, then use :+
//  def snocs(b: Buffer[A]): SnocSequence[A] = {
//    val n = Buffer.Arity - buf.size - 1
//    val buf2 = buf.snocs(b, n)
//    new SnocSequence()
//  }
//  */
//
//  private[util]
//  def toBuffer =
//    if (size > Buffer.Arity) sys.error("sequence bigger than buffer: " + size)
//    else if (hd.size == 0) buf
//    else hd
//
//  override def toString =
//    "SnocSequence(" + (0 until size.toInt).map(i => this(i.toLong)).mkString(", ") + ")"
//}
//
//object SnocSequence {
//
//  def empty[A]: SnocSequence[A] =
//    new SnocSequence[A](0L, Buffer.empty[A], null, Buffer.empty[A])
//
//  def single[A](a: A): SnocSequence[A] =
//    new SnocSequence[A](1L, Buffer.empty[A], null, Buffer.empty[A] :+ a)
//}

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
    def apply(i: Long) = elems(i)
    def ++(s: Sequence[A]): Sequence[A] =
      if (s.size + size < BufferSize * 2) Flat(elems ++ s.toDeque)
      else s match {
        case Flat(es2) => Nested(elems, Flat(Deque.empty), es2)
        case Nested(left, middle, right) =>
          if (elems.size < BufferSize) Nested(elems ++ left, middle, right)
          else Nested(elems, left +: middle, right)
      }
    def :+(a: A): Sequence[A] = Flat(elems :+ a)
    def +:(a: A): Sequence[A] = Flat(a +: elems)
    def size: Long = elems.size
    def toDeque = elems
    def take(n: Long) = Flat(elems.take(n))
    def drop(n: Long) = Flat(elems.drop(n))
    def uncons =
      if (elems.size == 0L) None
      else Some((elems(0), Flat(elems.drop(1))))
    def unsnoc =
      if (elems.size == 0L) None
      else Some((Flat(elems.take(elems.size - 1)), elems(elems.size - 1)))
  }

  /**
   * Nested sequence with a buffer on each side. Invariant is that no `Deque` is added
   * to `middle` unless its size >= BufferSize.
   */
  case class Nested[A](left: Deque[A], middle: Sequence[Deque[A]], right: Deque[A]) extends Sequence[A] {
    def :+(a: A): Sequence[A] = Nested(left, middle, right :+ a)
    def +:(a: A): Sequence[A] = Nested(a +: left, middle, right)

    def uncons =
      if (left.size > 0) Some(left(0) -> Nested(left drop 1, middle, right))
      else middle.uncons match {
        case None => Flat(right).uncons
        case Some((hd,middle)) => Some(hd(0) -> Nested(hd drop 1, middle, right))
      }

    def unsnoc = ???

    def take(n: Long) =
      if (n <= left.size) Flat(left take n)
      else if (n > left.size + middleSize) Flat(right take (n - (left.size + middleSize)))
      else middle.uncons match {
        case Some((mh, mt)) => Nested(mh, mt, right).take(n - left.size)
        case None => Flat(right).take(n - left.size)
      }

    def drop(n: Long) =
      if (n < left.size) Nested(left.drop(n), middle, right)
      else if (n > left.size + middleSize) Flat(right.drop(n - (left.size + middleSize)))
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
      if (i < left.size) left(i)
      else if (i >= left.size + middleSize) right(i - left.size - middleSize)
      else {
        var j = i - left.size
        var k = 0
        while (j >= middle(k).size) { j -= middle(k).size; k += 1 }
        middle(k)(j)
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

