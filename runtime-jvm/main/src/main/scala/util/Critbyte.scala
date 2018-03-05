package org.unisonweb.util

import Bytes.unsigned

sealed abstract class Critbyte[A] {
  def prefixedBy(key: Bytes.Seq): Critbyte[A]

  def lookup(key: Bytes.Seq): Option[A] = prefixedBy(key) match {
    case Critbyte.Leaf(Some((k,v))) if k == key => Some(v)
    case _ => None
  }

  def insert(key: Bytes.Seq, value: A): Critbyte[A]

  /** Insert (key, value), invoking `combine(old, value)` if there is already a value for the given `key`. */
  def insertAccumulate(key: Bytes.Seq, value: A)(combine: (A,A) => A): Critbyte[A] =
    lookup(key) match {
      case None => insert(key, value)
      case Some(old) => insert(key, combine(old,value))
    }
}

object Critbyte {

  private def Branch2[A](
    firstDiff: Int,
    smallestKey: Bytes.Seq,
    b1: Int,
    cb1: Critbyte[A],
    b2: Int,
    cb2: Critbyte[A]
  ): Critbyte[A] = {
    assert(b1 != b2)
    if (b1 > b2)
      Branch2(firstDiff, smallestKey, b2, cb2, b1, cb1)
    else {
      val a = new Array[Critbyte[A]](256)
      if (b1 == -1) {
        a(b2) = cb2
        Branch(firstDiff, smallestKey, cb1, a)
      }
      else {
        a(b1) = cb1
        a(b2) = cb2
        Branch(firstDiff, smallestKey, empty, a)
      }
    }
  }

  def empty[A]: Critbyte[A] = Leaf(None)

  def apply[A](kvs: (Bytes.Seq, A)*): Critbyte[A] =
    kvs.foldLeft(empty[A])((buf,kv) => buf.insert(kv._1, kv._2))

  case class Leaf[A](entry: Option[(Bytes.Seq, A)]) extends Critbyte[A] {
    def prefixedBy(key: Bytes.Seq) = entry match {
      case None => this
      case Some((k,v)) =>
        if (key.isPrefixOf(k)) this
        else empty
    }
    def insert(key: Bytes.Seq, value: A) = entry match {
      case None => Leaf(Some((key, value)))
      case Some((k,v)) =>
        try {
          val i = k.smallestDifferingIndex(key)
          Branch2(i, k min key, byteAt(i, k), this,
                                byteAt(i, key), Leaf(Some((key,value))))
        }
        catch { case Bytes.Seq.NotFound => Leaf(Some((k, value))) }
    }
  }

  private def byteAt(i: Int, b: Bytes.Seq): Int =
    try unsigned(b(i))
    catch { case Bytes.Seq.OutOfBounds => -1 }

  case class Branch[A](
      firstDiff: Int,
      smallestKey: Bytes.Seq,
      missingFirstDiff: Critbyte[A],
      children: Array[Critbyte[A]]) extends Critbyte[A] {

    def prefixedBy(key: Bytes.Seq) = {
      val child = children(unsigned(key(firstDiff)))
      if (child eq null) empty
      else child.prefixedBy(key)
    }
    def insert(key: Bytes.Seq, value: A) = {
      // `smallestKey` has more than `firstDiff` bytes
      // `key` may or may not.
      val newSmallestKey = smallestKey min key
      // `sdi` is either the index of the first byte that differs between
      //        `key` and `smallestKey`, or it is the length of `key`,
      //        (because `key` is shorter than `smallestKey`).
      val sdi = key smallestDifferingIndex smallestKey
      if (sdi < firstDiff) {
        // `key` differs from the children before they differ from each other
        Branch2(sdi, newSmallestKey, byteAt(sdi, key), Leaf(Some(key -> value)),
                                     byteAt(sdi, smallestKey), this)
      }
      else if (key.size == sdi) {
        // `key` doesn't exist at the position where children differ
        // also `sdi == firstDiff`
        Branch(sdi, newSmallestKey,
               missingFirstDiff.insert(key, value), children)
      }
      else {
        val bi = unsigned(key(firstDiff))
        val child = children(bi)
        val newChildren =
          children.updated(bi,
            if (child eq null)
              Leaf(Some((key, value)))
            else
              child.insert(key, value))
        Branch(firstDiff, newSmallestKey, missingFirstDiff, newChildren)
      }
    }
  }
}
