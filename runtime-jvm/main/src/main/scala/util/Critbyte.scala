package org.unisonweb.util

import Bytes.unsigned
import Critbyte._

sealed abstract class Critbyte[A] {

  /** Returns the submap of this `Critbyte` whose keys all have `key` as a prefix. */
  def prefixedBy(key: Bytes.Seq): Critbyte[A]

  // todo - think about optimized implementation
  def lookup(key: Bytes.Seq): Option[A] = prefixedBy(key) match {
    case Critbyte.Leaf(Some((k,v))) if k == key => Some(v)
    case Critbyte.Branch(fdi, smallestKey, runt, children) =>
      runt.lookup(key) orElse children.view.map(_.lookup(key)).find(_.isDefined).flatten
  }

  def insert(key: Bytes.Seq, value: A): Critbyte[A]

  /** Insert (key, value), invoking `combine(old, value)` if there is already a value for the given `key`. */
  def insertAccumulate(key: Bytes.Seq, value: A)(combine: (A,A) => A): Critbyte[A] =
    lookup(key) match {
      case None => insert(key, value)
      case Some(old) => insert(key, combine(old,value))
    }

  /** All keys in this map have this prefix. Satisfies `this.prefixedBy(prefix) == this`. */
  def prefix: Bytes.Seq

  def foldLeft[B](z: B)(f: (B,(Bytes.Seq,A)) => B): B

  /** Right-preferring union (if `key` exists in `this`, use its value). */
  def union(b: Critbyte[A]): Critbyte[A] = b.foldLeft(this)((buf, kv) => buf insert (kv._1, kv._2))
  // todo more efficient impl

  def isEmpty: Boolean = this match {
    case Leaf(None) => true
    case _ => false
  }
}

object Critbyte {

  private val emptyChildArray_ : Array[Critbyte[AnyRef]] = Array.fill(256)(empty)

  private def emptyChildArray[A]: Array[Critbyte[A]] = emptyChildArray_.asInstanceOf[Array[Critbyte[A]]]

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
      val a = emptyChildArray[A].clone
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
    def prefix = entry map (_._1) getOrElse Bytes.Seq.empty

    def foldLeft[B](z: B)(f: (B,(Bytes.Seq,A)) => B): B = entry.toList.foldLeft(z)(f)

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

    override def toString = this match {
      case Leaf(None) => "empty"
      case Leaf(Some((k,v))) => "(" + k + ", " + v + ")"
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

    lazy val prefix = smallestKey.take(firstDiff)

    def foldLeft[B](z: B)(f: (B,(Bytes.Seq,A)) => B): B =
      children.foldLeft(missingFirstDiff.foldLeft(z)(f))((b, child) => child.foldLeft(b)(f))

    def prefixedBy(key: Bytes.Seq) =
      // todo: does this have a more efficient implementation?
      if (key.isPrefixOf(prefix) || prefix.isPrefixOf(key))
        children.view.map(_.prefixedBy(key)).foldLeft(missingFirstDiff.prefixedBy(key))(_ union _)
      else
        empty

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
        val newChildren = children.updated(bi, child.insert(key, value))
        Branch(firstDiff, newSmallestKey, missingFirstDiff, newChildren)
      }
    }

    override def toString =
      s"Branch ($firstDiff $smallestKey [" +
        missingFirstDiff.toString + "] " +
        children.zipWithIndex.filterNot(_._1.isEmpty)
                .map(p => "" + p._2.toHexString + " " + p._1)
                .mkString(", ") +
      ")"
  }
}
