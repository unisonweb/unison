package org.unisonweb.util

import java.util.concurrent.atomic.AtomicInteger

/**
 * Buffer type with purely functional API, using mutable `Array` and cheap copy-on-write scheme.
 * Used as implementation detail by `Sequence`.
 *
 * Idea described by Bryan O'Sullivan in http://www.serpentine.com/blog/2014/05/31/attoparsec/
 */
private[util]
class Deque[A](idL: AtomicInteger, stampL: Int, val valuesL: Buffer[_,A], val sizeL: Int,
               idR: AtomicInteger, stampR: Int, val valuesR: Buffer[_,A], val sizeR: Int) {

  final val size = sizeL + sizeR

  /** Snoc a value onto the end of this `Deque`, possibly using (unobservable) mutation. */
  def :+(a: A): Deque[A] = {
    if (size == Int.MaxValue) throw Deque.Overflow
    val winner = idR.compareAndSet(stampR, stampR + 1)
    // threads race to be able to mutably update 'values', losers make a copy
    val valuesR2 = if (winner) valuesR :+ (sizeR, a) else valuesR.copy(0, sizeR) :+ (sizeR, a)
    if (winner && (valuesR2 eq valuesR))
      new Deque(idL, stampL, valuesL, sizeL, idR, stampR+1, valuesR2, sizeR + 1)
    else
      new Deque(idL, stampL, valuesL, sizeL, new AtomicInteger(Deque.MinStamp), Deque.MinStamp, valuesR2, sizeR + 1)
  }

  /** Cons a value onto the front of this `Deque`, possibly using (unobservable) mutation. */
  def +:(a: A): Deque[A] = {
    if (size == Int.MaxValue) throw Deque.Overflow
    val winner = idL.compareAndSet(stampL, stampL + 1)
    // threads race to be able to mutably update 'values', losers make a copy
    val valuesL2 = if (winner) valuesL :+ (sizeL, a) else valuesL.copy(0, sizeL) :+ (sizeL, a)
    if (winner && (valuesL2 eq valuesL))
      new Deque(idL, stampL+1, valuesL2, sizeL + 1, idR, stampR, valuesR, sizeR)
    else
      new Deque(new AtomicInteger(Deque.MinStamp), Deque.MinStamp, valuesL2, sizeL + 1, idR, stampR, valuesR, sizeR)
  }

  private def dumbAppend(b: Deque[A]): Deque[A] = {
    if (Int.MaxValue - size - b.size < 0) throw Deque.Overflow
    if (b.sizeL == 0 && b.sizeR != 0) {
      val winner = idR.compareAndSet(stampR, stampR + 1)
      // threads race to be able to mutably update
      val valuesR2 = if (winner) b.valuesR.copyTo(sizeR, valuesR, b.sizeR)
                     else        b.valuesR.copyTo(sizeR, valuesR.copy(0, sizeR), b.sizeR)
      if (winner && (valuesR2 eq valuesR))
        new Deque(idL, stampL, valuesL, sizeL, idR, stampR+1, valuesR2, sizeR + b.sizeR)
      else
        new Deque(idL, stampL, valuesL, sizeL, new AtomicInteger(Deque.MinStamp), Deque.MinStamp, valuesR2, sizeR + b.sizeR)
    }
    else if (b.isEmpty) this
    else
      b.take(b.sizeL).foldLeft(this)(_ :+ _).dumbAppend(b.drop(b.sizeL))
  }

  def ++(b: Deque[A]): Deque[A] =
    if (size < b.size / 2) this.foldRight(b)(_ +: _)
    else this.dumbAppend(b)

  def reverse: Deque[A] =
    if (size < 2) this
    else new Deque(idR, stampR, valuesR, sizeR, idL, stampL, valuesL, sizeL) // nifty! just swap roles of left and right sides

  def foldRight[B](z: B)(f: (A,B) => B): B =
    ((size - 1) to 0 by -1).foldLeft(z)((z,i) => f(apply(i), z))

  def foldLeft[B](z: B)(f: (B,A) => B): B =
    (0 until size).foldLeft(z)((b,i) => f(b, apply(i)))

  def take(n: Int): Deque[A] =
    if (n >= size) this
    else if (n <= 0) Deque.fromBuffer(valuesR.empty, 0)
    else if (n >= sizeL) // keep whole left side, take partial of right side
      new Deque(idL, stampL, valuesL, sizeL, idR, Int.MinValue, valuesR, n - sizeL)
    else { // remove the whole right side
      val valuesL2 = valuesL.copy(sizeL - n, n)
      new Deque(
        new AtomicInteger(Deque.MinStamp), Deque.MinStamp, valuesL2, n,
        new AtomicInteger(Deque.MinStamp), Deque.MinStamp, valuesR.empty, 0
      )
    }

  def drop(n: Int): Deque[A] =
    if (n >= size) Deque.fromBuffer(valuesR.empty, 0)
    else if (n <= 0) this
    else if (n <= sizeL)
      new Deque(new AtomicInteger(Deque.MinStamp), Int.MinValue, valuesL, sizeL - n,
                idR, stampR, valuesR, sizeR)
    else { // n >= sizeL, remove the whole left side
      val sizeR2 = sizeR - (n - sizeL)
      val valuesR2 = valuesR.copy(n - sizeL, sizeR2)
      new Deque(
        new AtomicInteger(Deque.MinStamp), Deque.MinStamp, valuesR2.empty, 0,
        new AtomicInteger(Deque.MinStamp), Deque.MinStamp, valuesR2, sizeR2)
    }

  def dropRight(n: Int): Deque[A] = take(size - n)

  def apply(i: Int): A =
    if (i < sizeL) valuesL(sizeL - i - 1) // valuesL is in reverse order
    else valuesR(i - sizeL)

  def length = size

  final def isEmpty = size == 0

  def map[B](f: A => B): Deque[B] = {
    var buf = Deque.empty[B]
    var i = 0; while (i < size) { buf = buf :+ f(apply(i)); i += 1 }
    buf
  }

  def toVector =
    (0 until size).map(apply(_)).toVector

  override def hashCode = (0 until size).map(apply(_)).hashCode
  override def equals(d0: Any) = {
    val d = d0.asInstanceOf[Deque[A]]
    d.size == size && (0 until size).forall(i => apply(i) == d(i))
  }

  override def toString =
    "Deque(" + (0 until size).map(apply(_)).mkString(", ") + ")"
}

object Deque {

  case object Overflow extends Throwable { override def fillInStackTrace = this }

  private val MinStamp = Int.MinValue + 1

  def fromBuffer[A](b: Buffer[_,A], sizeR: Int): Deque[A] =
    new Deque(new AtomicInteger(MinStamp), MinStamp, b.empty, 0,
              new AtomicInteger(MinStamp), MinStamp, b, sizeR)

  def fromArray[A](a: Array[A])(implicit A: Buffer.NewArray[A]): Deque[A] =
    fromBuffer(Buffer.fromArray(a), a.length)

  def empty[A]: Deque[A] = fromBuffer(Buffer.empty[A], 0)

  def single[A](a: A): Deque[A] = fromBuffer(Buffer.empty[A] :+ (0, a), 1)

  def apply[A](as: A*): Deque[A] =
    as.foldLeft(empty[A])(_ :+ _)
}
