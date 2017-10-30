package org.unisonweb.util

class SnocSequence[A](val size: Long, hd: Buffer[A], tl: SnocSequence[Buffer[A]], buf: Buffer[A]) {

  private final def tlSize = if (tl eq null) 0 else tl.size * Buffer.Arity
  private final val hdSizePlusTlSize = hd.size + tlSize

  def apply(i: Long): A = {
    if (i < hd.size) hd(i)
    else if (i < hdSizePlusTlSize) tl((i - Buffer.Arity) / Buffer.Arity)(i % Buffer.Arity)
    else buf(i - hdSizePlusTlSize)
  }

  def :+(a: A): SnocSequence[A] = (buf :+ a) match { case buf =>
    if (buf.size != Buffer.Arity) new SnocSequence(size + 1, hd, tl, buf)
    else if (size == Buffer.Arity - 1) new SnocSequence(size + 1, buf, tl, Buffer.empty[A])
    else new SnocSequence(size + 1, hd,
                          if (tl eq null) SnocSequence.single(buf) else tl :+ buf,
                          Buffer.empty[A])
  }

  override def toString =
    "SnocSequence(" + (0 until size.toInt).map(i => this(i.toLong)).mkString(", ") + ")"
}

object SnocSequence {

  def empty[A]: SnocSequence[A] =
    new SnocSequence[A](0L, Buffer.empty[A], null, Buffer.empty[A])

  def single[A](a: A): SnocSequence[A] =
    new SnocSequence[A](1L, Buffer.empty[A], null, Buffer.empty[A] :+ a)
}

abstract class Sequence[A] {
  def apply(i: Long): A
  def ++(s: Sequence[A]): Sequence[A]
  def :+(a: A): Sequence[A]
  def +:(a: A): Sequence[A]
  def size: Long

  override def toString = "Sequence(" + (0L until size).map(apply(_)).mkString(", ") + ")"

  override lazy val hashCode = (0L until size).map(apply(_)).hashCode
  override def equals(other: Any) = {
    val s2 = other.asInstanceOf[Sequence[A]]
    size == s2.size && (0L until size).forall(i => apply(i) == s2(i))
  }

  def foldLeft[B](z: B)(f: (B,A) => B): B =
    (0L until size).foldLeft(z)((b,i) => f(b, apply(i)))

  private[util] def depth: Long
  final def isEmpty = size == 0L
}

object Sequence {

  def single[A](a: A): Sequence[A] =
    Snoc(SnocSequence.single(a))

  def empty[A]: Sequence[A] =
    Snoc(SnocSequence.empty)

  def apply[A](as: A*): Sequence[A] =
    Snoc(as.foldLeft(SnocSequence.empty[A])((buf,a) => buf :+ a))

  case class Cons[A](s: SnocSequence[A]) extends Sequence[A] {
    def apply(i: Long): A = s(s.size - i - 1)
    def ++(s2: Sequence[A]): Sequence[A] = Append(this, s2)
    def :+(a: A): Sequence[A] = Append(this, Snoc(SnocSequence.single(a)))
    def +:(a: A): Sequence[A] = Cons(s :+ a)
    def size: Long = s.size
    def depth = 0
  }

  case class Snoc[A](s: SnocSequence[A]) extends Sequence[A] {
    def apply(i: Long): A = s(i)
    def ++(s2: Sequence[A]): Sequence[A] = Append(this, s2)
    def :+(a: A): Sequence[A] = Snoc(s :+ a)
    def +:(a: A): Sequence[A] = Append(Cons(SnocSequence.single(a)), this)
    def size: Long = s.size
    def depth = 0
  }

  case class Append[A](left: Sequence[A], right: Sequence[A]) extends Sequence[A] { self =>
    def apply(i: Long): A =
      if (i < left.size) left(i)
      else right(i - left.size)

    def ++(s2: Sequence[A]): Sequence[A] =
      if (s2.size < 16) Append(left, (0L until s2.size).foldLeft(right)((right, i) => right :+ s2(i)))
      else rebalanceAppend(s2)

    private
    def rebalanceAppend(s2: Sequence[A]): Sequence[A] =
      if (s2.isEmpty) this
      else if (this.isEmpty) s2
      else {
        @annotation.tailrec
        def go(chunks: Append[A], last: Sequence[A]): Sequence[A] = {
          val lastN = last.size
          if (lastN >= chunks.size || lastN*2 <= chunks.right.size)
            Append(chunks, last)
          else chunks.left match {
            case left @ Append(_,_) => go(left, Append(chunks.right, last))
            case _ => Append(chunks, last)
          }
        }
        go(this, s2)
      }

    def :+(a: A): Sequence[A] = Append(left, right :+ a)
    def +:(a: A): Sequence[A] = Append(a +: left, right)
    val size: Long = left.size + right.size
    val depth = (left.depth max right.depth) + 1
  }
}

