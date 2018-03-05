package org.unisonweb.util

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
          val arr = new Array[Critbyte[A]](256)
          arr(key(i)) = Leaf(Some((key,value)))
          arr(k(i)) = this
          Branch(i, arr)
        }
        catch { case Bytes.Seq.NotFound => Leaf(Some((k, value))) }
    }
  }

  case class Branch[A](i: Int, children: Array[Critbyte[A]]) extends Critbyte[A] {
    def prefixedBy(key: Bytes.Seq) = {
      val child = children(key(i))
      if (child eq null) empty
      else child.prefixedBy(key)
    }
    def insert(key: Bytes.Seq, value: A) = {
      val children2 = children.clone
      val bi = key(i)
      val child = children2(bi).insert(key, value)
      children2(bi) = child
      Branch(i, children2)
    }
  }
}
