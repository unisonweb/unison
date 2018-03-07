package org.unisonweb.util

import Bytes.unsigned
import Critbyte._

sealed abstract class Critbyte[A] {

  /** Returns the submap of this `Critbyte` whose keys all have `key` as a prefix. */
  def prefixedBy(key: Bytes.Seq): Critbyte[A]

  def lookup(key: Bytes.Seq): Option[A]

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

  def remove(key: Bytes.Seq): Critbyte[A]

  /** List the keys in lexicographical order */
  def keys: List[Bytes.Seq] = foldLeft(List[Bytes.Seq]()) {
    case (bs, (k, _)) => k :: bs
  }.reverse
}

object Critbyte {

  private val emptyLeaf_ = Leaf[Any](None)

  private def leaf[A](k: Bytes.Seq, v: A) = Leaf(Some(k -> v))

  def empty[A]: Critbyte[A] = emptyLeaf

  private def emptyLeaf[A]: Leaf[A] = emptyLeaf_.asInstanceOf[Leaf[A]]

  val emptyChildArray_ : Array[Critbyte[AnyRef]] = Array.fill(256)(empty)

  private def emptyChildArray[A]: Array[Critbyte[A]] = emptyChildArray_.asInstanceOf[Array[Critbyte[A]]]

  private def Branch2[A](
    critbyte: Int,
    smallestKey: Bytes.Seq,
    b1: Int,
    cb1: Leaf[A],
    b2: Int,
    cb2: Critbyte[A]
  ): Critbyte[A] = {
    if (b1 < b2) {
      val a = emptyChildArray[A].clone
      if (b1 == -1) {
        a(b2) = cb2
        Branch(critbyte, smallestKey, cb1, a)
      }
      else {
        a(b1) = cb1
        a(b2) = cb2
        Branch(critbyte, smallestKey, emptyLeaf, a)
      }
    } else {
      cb2 match {
        case us@Leaf(_) => Branch2(critbyte, smallestKey, b2, us, b1, cb1)
        case _ => ??? // Should never happen
      }
    }
  }

  def apply[A](kvs: (Bytes.Seq, A)*): Critbyte[A] =
    kvs.foldLeft(empty[A])((buf,kv) => buf.insert(kv._1, kv._2))

  case class Leaf[A](entry: Option[(Bytes.Seq, A)]) extends Critbyte[A] {

    override def lookup(key: Bytes.Seq) = entry match {
      case Some((k,v)) if k == key => Some(v)
      case _ => None
    }

    def prefix = entry map (_._1) getOrElse Bytes.Seq.empty

    def foldLeft[B](z: B)(f: (B,(Bytes.Seq,A)) => B): B =
      entry.toList.foldLeft(z)(f)

    def prefixedBy(key: Bytes.Seq) = entry match {
      case None => this
      case Some((k,v)) =>
        if (key.isPrefixOf(k)) this
        else empty
    }

    def insert(key: Bytes.Seq, value: A) = entry match {
      case None => leaf(key, value)
      case Some((k,v)) =>
        try {
          val i = k.smallestDifferingIndex(key)
          if (k.size == i) {
            assert(key.size > i)
            // this becomes the runt
            Branch(i, k, this,
                   emptyChildArray[A].updated(unsigned(key(i)),
                                              leaf(key, value)))
          }
          else if (key.size == i) {
            assert(k.size > i)
            // the new leaf becomes the runt
            Branch(i, key,
                   leaf(key, value),
                   emptyChildArray[A].updated(unsigned(k(i)), this))
          } else {
            // There's no runt
            assert(k(i) != key(i))
            val chirren = emptyChildArray[A].clone
            chirren(unsigned(k(i))) = leaf(k, v)
            chirren(unsigned(key(i))) = leaf(key, value)
            Branch(i, key min k, emptyLeaf, chirren)
          }
        }
        catch { case Bytes.Seq.NotFound => leaf(k, value) }
    }

    def remove(key: Bytes.Seq) = entry match {
      case Some((k,v)) if k == key => empty
      case _ => this
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
      critbyte: Int,
      smallestKey: Bytes.Seq,
      runt: Leaf[A],
      children: Array[Critbyte[A]]) extends Critbyte[A] {

    lazy val prefix = smallestKey.take(critbyte)

    def foldLeft[B](z: B)(f: (B,(Bytes.Seq,A)) => B): B =
      children.foldLeft(runt.foldLeft(z)(f))((b, child) => child.foldLeft(b)(f))

    def lookup(key: Bytes.Seq) =
      if (key.isPrefixOf(prefix) || prefix.isPrefixOf(key)) {
        // lookup(key) on all children (including runt), return first non-None match
        (Iterator.single(runt) ++ children.iterator).
          map(_.lookup(key)).find(_.isDefined).flatten
      }
      else None

    def prefixedBy(key: Bytes.Seq) =
      if (key.size == 0) this
      else {
        // todo: does this have a more efficient implementation?
        if (key.isPrefixOf(prefix) || prefix.isPrefixOf(key))
          children.view.map(_.prefixedBy(key)).foldLeft(runt.prefixedBy(key))(_ union _)
        else
          empty
      }

    def insert(key: Bytes.Seq, value: A) = {
      // `smallestKey` has more than `critbyte` bytes
      // `key` may or may not.
      val newSmallestKey = smallestKey min key
      // `sdi` is either the index of the first byte that differs between
      //        `key` and `smallestKey`, or it is the length of `key`,
      //        (because `key` is shorter than `smallestKey`).
      val sdi = key smallestDifferingIndex smallestKey

      if (sdi >= critbyte) {
        // The new key belongs in this branch
        if (key.size == critbyte) {
          assert(runt.entry.forall(_._1 == key))
          copy(smallestKey = newSmallestKey, runt = leaf(key, value))
        } else {
          assert(key.size > critbyte)
          val critValue = unsigned(key(critbyte))
          copy(children =
            children.updated(critValue, children(critValue).insert(key, value)))
        }
      } else {
        // The new key has an earlier critbyte and we need a new top-level
        if (key.size == sdi) {
          Branch(sdi, newSmallestKey, leaf(key, value),
                 emptyChildArray[A].updated(unsigned(smallestKey(sdi)), this))
        } else {
          assert(sdi < key.size)
          val chirren = emptyChildArray[A].clone
          chirren(unsigned(smallestKey(sdi))) = this
          chirren(unsigned(key(sdi))) = leaf(key, value)
          Branch(sdi, newSmallestKey, emptyLeaf[A], chirren)
        }
      }
    }

    def remove(key: Bytes.Seq) =
      if (key.isPrefixOf(prefix) || prefix.isPrefixOf(key))
        children.view.map(_.remove(key)).foldLeft(runt.remove(key))(_ union _)
      else this

    override def toString =
      s"Branch ($critbyte $smallestKey [" +
        runt.toString + "] " +
        children.zipWithIndex.filterNot(_._1.isEmpty)
                .map(p => "" + p._2.toHexString + " " + p._1)
                .mkString(", ") +
      ")"
  }
}
