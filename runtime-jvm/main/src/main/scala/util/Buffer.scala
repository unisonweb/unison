package org.unisonweb.util

import java.util.concurrent.atomic.AtomicLong
// import scala.reflect.ClassTag

/**
 * Buffer type with purely functional API, using mutable `Array` and cheap copy-on-write scheme.
 * Used as implementation detail by `Sequence`.
 *
 * Idea described by Bryan O'Sullivan in http://www.serpentine.com/blog/2014/05/31/attoparsec/
 */
private[util]
class Buffer[A](id: AtomicLong, stamp: Long, val values: Array[A], val size: Int) {

  /** Snoc a value onto the end of this `Buffer`, possibly using (unobservable) mutation. */
  def :+(a: A): Buffer[A] = {
    val winner = id.compareAndSet(stamp, stamp+1)
    val values2 =
      if (size == values.length) {
        val values2 = (new Array[Any](values.length * 2)).asInstanceOf[Array[A]]
        values.copyToArray(values2)
        values2
      }
      // threads race to be able to mutably update 'values'
      else if (winner) values
      // losers forced to make a copy
      else values.clone
    values2(size) = a
    if (winner && (values2 eq values)) new Buffer(id, stamp+1, values2, size+1)
    else new Buffer(new AtomicLong(0), 0, values2, size + 1)
  }

  def ++(b: Buffer[A]): Buffer[A] =
    if (b.isEmpty) this
    else {
      val winner = id.compareAndSet(stamp, stamp+1)
      val newsize = size + b.size
      val values2 =
        if (newsize >= values.length) {
          val values2 = (new Array[Any]((values.length * 2) max newsize)).asInstanceOf[Array[A]]
          values.copyToArray(values2)
          values2
        }
        // threads race to be able to mutably update 'values'
        else if (winner) values
        // losers forced to make a copy
        else values.clone
      b.values.copyToArray(values2, size, b.size)
      if (winner && (values2 eq values)) new Buffer(id, stamp+1, values2, newsize)
      else new Buffer(new AtomicLong(0), 0, values2, newsize)
    }

  def reverse: Buffer[A] =
    if (size < 2) this
    else ((size - 1) to 0 by -1).foldLeft(Buffer.empty[A])((buf,i) => buf :+ apply(i))

  def foldLeft[B](z: B)(f: (B,A) => B): B =
    (0 until size).foldLeft(z)((b,i) => f(b, values(i)))

  def take(n: Int): Buffer[A] =
    if (n >= size) this
    else new Buffer(id, -1, values, size min n)

  def drop(n: Int): Buffer[A] =
    if (n >= size) Buffer.empty
    else {
      val values2 = (new Array[Any](values.length)).asInstanceOf[Array[A]]
      Array.copy(values, n, values2, 0, size - n)
      new Buffer(new AtomicLong(0), -1, values2, (size - n) max 0)
    }

  def dropRight(n: Int): Buffer[A] = take(size - n)

  def apply(index: Long): A = values(index.toInt)

  def length = size
  final def isEmpty = size == 0L

  override def toString = "Buffer(" ++ values.iterator.take(size).mkString(", ") ++ ")"
}

object Buffer {

  private val ArityI = 128
  val Arity = 128L

  def empty[A]: Buffer[A] =
    new Buffer(new AtomicLong(0), 0, new Array[Any](ArityI), 0).asInstanceOf[Buffer[A]]

  def single[A](a: A): Buffer[A] = {
    val arr = new Array[Any](ArityI)
    arr(0) = a : Any
    new Buffer(new AtomicLong(0), 0, arr, 1).asInstanceOf[Buffer[A]]
  }

  def apply[A](as: A*): Buffer[A] = {
    val underlying = as.toArray[Any]
    val paddedUnderlying =
      if (underlying.length < ArityI) underlying ++ new Array[Any](ArityI - underlying.length)
      else underlying
    new Buffer(new AtomicLong(0), 0, paddedUnderlying, as.length).asInstanceOf[Buffer[A]]
  }

  /*
   * Returns the smallest index, i, into sizes such that `vs(i) >= n`,
   * assuming `vs` is sorted. If `n` exceeds the maximum value in `vs`,
   * throws an exception. This takes time `O(lg i)`, so searches which
   * succeed near the front of `vs` return more quickly.
   */
  def search(vs: Buffer[Long], n: Long): Int = {

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

class Deque[A](val left: Buffer[A], val right: Buffer[A]) {
  def apply(i: Long): A =
    if (i < left.size) left(left.size - i - 1)
    else right(i - left.size)

  def :+(a: A): Deque[A] = new Deque(left, right :+ a)
  def +:(a: A): Deque[A] = new Deque(left :+ a, right)

  def ++(d: Deque[A]): Deque[A] =
    new Deque(left, right ++ d.left.reverse ++ d.right)

  def take(n: Long): Deque[A] =
    if (n >= size) this
    else if (n < left.size) new Deque(Buffer.empty, left.reverse.take(n.toInt))
    else new Deque(left, right.take(n.toInt - left.size))

  def drop(n: Long): Deque[A] =
    if (n >= size) Deque.empty
    else if (n < left.size) new Deque(left.dropRight(n.toInt), right)
    else (new Deque(right.reverse, Buffer.empty)).drop(n - left.size)

  def size = left.size + right.size

  override def toString = "Deque(" + (0L until size).map(apply(_)).mkString(", ") + ")"
}

object Deque {

  def empty[A]: Deque[A] =
    new Deque(Buffer.empty[A], Buffer.empty[A])

  def single[A](a: A): Deque[A] =
    new Deque(Buffer.empty[A], Buffer.single(a))
}
