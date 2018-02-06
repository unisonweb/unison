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
}
