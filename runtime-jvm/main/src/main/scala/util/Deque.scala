package org.unisonweb.util

import java.util.concurrent.atomic.AtomicInteger
// import scala.reflect.ClassTag

/**
 * Buffer type with purely functional API, using mutable `Array` and cheap copy-on-write scheme.
 * Used as implementation detail by `Sequence`.
 *
 * Idea described by Bryan O'Sullivan in http://www.serpentine.com/blog/2014/05/31/attoparsec/
 */
private[util]
class Deque[A](idL: AtomicInteger, stampL: Int, val valuesL: Array[A], val sizeL: Int,
               idR: AtomicInteger, stampR: Int, val valuesR: Array[A], val sizeR: Int) {

  final def size = sizeL + sizeR

  /** Snoc a value onto the end of this `Deque`, possibly using (unobservable) mutation. */
  def :+(a: A): Deque[A] = {
    if (size == Int.MaxValue) throw Deque.Overflow
    val winner = idR.compareAndSet(stampR, stampR + 1)
    val valuesR2 =
      if (sizeR == valuesR.length) {
        val valuesR2 = (new Array[Any](valuesR.length * 2)).asInstanceOf[Array[A]]
        valuesR.copyToArray(valuesR2)
        valuesR2
      }
      // threads race to be able to mutably update 'values'
      else if (winner) valuesR
      // losers forced to make a copy
      else valuesR.clone
    valuesR2(sizeR) = a
    if (winner && (valuesR2 eq valuesR))
      new Deque(idL, stampL, valuesL, sizeL, idR, stampR+1, valuesR2, sizeR+1)
    else
      new Deque(idL, stampL, valuesL, sizeL, new AtomicInteger(Deque.MinStamp), Deque.MinStamp, valuesR2, sizeR + 1)
  }

  /** Cons a value onto the front of this `Deque`, possibly using (unobservable) mutation. */
  def +:(a: A): Deque[A] = {
    if (size == Int.MaxValue) throw Deque.Overflow
    val winner = idL.compareAndSet(stampL, stampL + 1)
    val valuesL2 =
      if (sizeL == valuesL.length) {
        val valuesL2 = (new Array[Any](valuesL.length * 2)).asInstanceOf[Array[A]]
        valuesL.copyToArray(valuesL2)
        valuesL2
      }
      // threads race to be able to mutably update 'values'
      else if (winner) valuesL
      // losers forced to make a copy
      else valuesL.clone
    valuesL2(sizeL) = a
    if (winner && (valuesL2 eq valuesL))
      new Deque(idL, stampL+1, valuesL2, sizeL+1, idR, stampR, valuesR, sizeR)
    else
      new Deque(new AtomicInteger(Deque.MinStamp), Deque.MinStamp, valuesL2, sizeL + 1, idR, stampR, valuesR, sizeR)
  }

  private def dumbAppend(b: Deque[A]): Deque[A] = {
    if (Int.MaxValue - size - b.size < 0) throw Deque.Overflow
    if (b.sizeL == 0 && b.sizeR != 0) {
      val winner = idR.compareAndSet(stampR, stampR + 1)
      val valuesR2 =
        if (valuesR.length - sizeR - b.sizeR <= 0) {
          val valuesR2 = (new Array[Any]((valuesR.length * 2) max (sizeR + b.sizeR))).asInstanceOf[Array[A]]
          valuesR.copyToArray(valuesR2)
          valuesR2
        }
        // threads race to be able to mutably update 'values'
        else if (winner) valuesR
        // losers forced to make a copy
        else valuesR.clone
      Array.copy(b.valuesR, 0, valuesR2, sizeR, b.sizeR)
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
    else ((size - 1) to 0 by -1).foldLeft(Deque.empty[A])((buf,i) => buf :+ apply(i))

  def foldRight[B](z: B)(f: (A,B) => B): B =
    ((size - 1) to 0 by -1).foldLeft(z)((z,i) => f(apply(i), z))

  def foldLeft[B](z: B)(f: (B,A) => B): B =
    (0 until size).foldLeft(z)((b,i) => f(b, apply(i)))

  def take(n: Int): Deque[A] =
    if (n >= size) this
    else if (n >= sizeL)
      new Deque(idL, stampL, valuesL, sizeL,
                idR, Int.MinValue, valuesR, sizeR min (n - sizeL))
    else { // remove the whole right side
      val valuesL2 = (new Array[Any](valuesL.length)).asInstanceOf[Array[A]]
      val sizeL2 = sizeL - n
      Array.copy(valuesL, n, valuesL2, 0, sizeL2)
      new Deque(
        new AtomicInteger(Deque.MinStamp), Deque.MinStamp, valuesL2, sizeL2,
        new AtomicInteger(Deque.MinStamp), Deque.MinStamp, (new Array[Any](Deque.ArityI)).asInstanceOf[Array[A]], 0
      )
    }

  def drop(n: Int): Deque[A] =
    if (n >= size) Deque.empty
    else if (n <= sizeL)
      new Deque(new AtomicInteger(Deque.MinStamp), Int.MinValue, valuesL, sizeL - n,
                idR, stampR, valuesR, sizeR)
    else { // n >= sizeL, remove the whole left side
      val valuesR2 = (new Array[Any](valuesR.length)).asInstanceOf[Array[A]]
      val sizeR2 = sizeR - (n - sizeL)
      Array.copy(valuesR, n, valuesR2, 0, sizeR2)
      new Deque(
        new AtomicInteger(Deque.MinStamp), Deque.MinStamp, new Array[Any](Deque.ArityI).asInstanceOf[Array[A]], 0,
        new AtomicInteger(Deque.MinStamp), Deque.MinStamp, valuesR2, sizeR2)
    }

  def dropRight(n: Int): Deque[A] = take(size - n)

  def apply(index: Int): A =
    if (index < sizeL) valuesL(sizeL - index.toInt - 1) // valuesL is in reverse order
    else valuesR(index.toInt - sizeL)

  def length = size

  final def isEmpty = sizeL == 0 && sizeR == 0

  override def toString =
    "Deque(" + (0 until size).map(apply(_)).mkString(", ") + ")"
}

object Deque {

  case object Overflow extends Throwable { override def fillInStackTrace = this }

  private val ArityI = 16
  private val MinStamp = Int.MinValue + 1

  def empty[A]: Deque[A] =
    new Deque(
      new AtomicInteger(MinStamp), MinStamp, new Array[Any](ArityI), 0,
      new AtomicInteger(MinStamp), MinStamp, new Array[Any](ArityI), 0).asInstanceOf[Deque[A]]

  def single[A](a: A): Deque[A] = {
    val arr = new Array[Any](ArityI)
    arr(0) = a : Any
    new Deque(
      new AtomicInteger(MinStamp), MinStamp, new Array[Any](ArityI), 0,
      new AtomicInteger(MinStamp), MinStamp, arr, 1).asInstanceOf[Deque[A]]
  }

  def apply[A](as: A*): Deque[A] = {
    val underlying = as.toArray[Any]
    val paddedUnderlying =
      if (underlying.length < ArityI) underlying ++ new Array[Any](ArityI - underlying.length)
      else underlying
    new Deque(
      new AtomicInteger(MinStamp), MinStamp, new Array[Any](ArityI), 0,
      new AtomicInteger(MinStamp), MinStamp, paddedUnderlying, as.length).asInstanceOf[Deque[A]]
  }

  /*
   * Returns the smallest index, i, into sizes such that `vs(i) >= n`,
   * assuming `vs` is sorted. If `n` exceeds the maximum value in `vs`,
   * throws an exception. This takes time `O(lg i)`, so searches which
   * succeed near the front of `vs` return more quickly.
   */
  def search(vs: Deque[Long], n: Long): Int = {

    // progressively double index, starting from 0, until
    // hitting an index whose value exceeds `n`, then use
    // the remaining range for a binary search
    @annotation.tailrec
    def doublingSearch(prev: Int, current: Int): Int =
      if (current > vs.size || vs(current - 1) >= n)
        binarySearch(prev - 1, math.min(current, vs.size) - 1)
      else
        doublingSearch(current, current * 2)

    @annotation.tailrec
    def binarySearch(low: Int, high: Int): Int =
      if (low <= high) {
        val mid = (low + high) >>> 1
        val v = vs(mid)
        if (v < n) binarySearch(mid+1, high)
        else if (v > n) binarySearch(low, mid-1)
        else mid
      }
      else if (low >= vs.size)
        throw new IllegalArgumentException(s"value $n greater than max ${vs(vs.size - 1)}")
      else low

    doublingSearch(1, 1)
  }
}
