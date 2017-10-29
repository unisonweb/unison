package org.unisonweb.util

import java.util.concurrent.atomic.AtomicLong
import scala.reflect.ClassTag

/**
 * Buffer type with purely functional API, using mutable `Array` and cheap copy-on-write scheme.
 * Buffer cannot grow beyond its max size (currently 64). Used as implementation detail by
 * `Sequence`.
 *
 * Idea described by Bryan O'Sullivan in http://www.serpentine.com/blog/2014/05/31/attoparsec/
 */
private[util]
class Buffer[A](id: AtomicLong, stamp: Long, values: Array[A], val size: Int) {

  /** Snoc a value onto the end of this `Buffer`, possibly using (unobservable) mutation. */
  def :+(a: A): Buffer[A] =
    // threads race to be able to mutably update 'values'
    if (id.compareAndSet(stamp, stamp+1)) {
      values(size) = a
      new Buffer(id, stamp+1, values, size+1)
    }
    else // losers are forced to make a copy of the buffer
      new Buffer(new AtomicLong(0), 0, { val vs2 = values.clone; vs2(size) = a; vs2 }, size + 1)

  def apply(index: Long): A = values(index.toInt)

  def length = size

  override def toString = "Buffer(" ++ values.iterator.take(size).mkString(", ") ++ ")"
}

object Buffer {

  val Arity = 64

  def empty[A:ClassTag]: Buffer[A] = new Buffer(new AtomicLong(0), 0, new Array[A](Arity), 0)

  def apply[A:ClassTag](as: A*): Buffer[A] = {
    val underlying = as.toArray
    val paddedUnderlying =
      if (underlying.length < Arity) underlying ++ new Array[A](Arity - underlying.length)
      else underlying
    new Buffer(new AtomicLong(0), 0, paddedUnderlying, as.length)
  }
}
