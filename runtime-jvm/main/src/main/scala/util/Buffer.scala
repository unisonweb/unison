package org.unisonweb.util

sealed abstract class Buffer[T,Elem] {
  /** Create a copy of `len` elements of this `Buffer`, starting from index `from`. */
  def copy(from: Int, len: Int): Buffer[T,Elem]

  /** Set the `i` index of this `Buffer`, extending the backing storage if needed. */
  def :+(i: Int, e: Elem): Buffer[T,Elem]

  /** Copy `len` elements of es to the `i` index of this `Buffer`. */
  def :++(i: Int, es: Array[Elem], len: Int): Buffer[T,Elem]

  /** Copy `len` elements of this `Buffer` to the `i` position of `b2`. */
  def copyTo[T2](i: Int, b2: Buffer[T2,Elem], len: Int): Buffer[T2,Elem]

  /** The element at the provided index. */
  def apply(i: Int): Elem

  /** Create an empty version of this same `Buffer`, using the same kind of backing storage. */
  def empty: Buffer[T,Elem]

  /** Convert the first `size` elements of this `Buffer` to an array. */
  def toArray(size: Int): Array[Elem]
}

object Buffer {

  def viewArray[A](arr: Array[A])(implicit newArray: NewArray[A]): Buffer[Array[A],A] = new Buffer[Array[A],A] {
    def copy(from: Int, len: Int) = {
      val arr2 = newArray(len)
      Array.copy(arr, from, arr2, 0, arr2.length)
      fromArray(arr2)
    }
    def copyTo[T2](i: Int, b2: Buffer[T2,A], len: Int) = b2 :++ (i, arr, len)

    def apply(i: Int) = arr(i)

    def :++(i: Int, src: Array[A], len: Int) = {
      val arr2 = if (i + len >= arr.length) {
        val arr2 = newArray((arr.length * 2) max (len + i))
        Array.copy(arr, 0, arr2, 0, i)
        arr2
      } else arr
      Array.copy(src, 0, arr2, i, len)
      if (arr2 eq arr) this else viewArray(arr2)
    }

    def :+(i: Int, a: A) = {
      val arr2 =
        if (i >= arr.length) {
          val arr2 = newArray((arr.length * 2) max (i + 1))
          Array.copy(arr, 0, arr2, 0, arr.length)
          arr2
        }
        else arr
      arr2(i) = a
      if (arr2 eq arr) this else viewArray(arr2)
    }

    def empty = fromArray(newArray(16))
    def toArray(size: Int) = { val r = newArray(size); Array.copy(arr, 0, r, 0, r.length); r }
  }

  def fromArray[A](arr: Array[A])(implicit newArray: NewArray[A]): Buffer[Array[A],A] =
    viewArray(arr.clone)

  def empty[A](implicit newArray: NewArray[A]): Buffer[Array[A],A] =
    viewArray(newArray(16))

  /*
  represent a string as a Java string
  plus an Array[Int] of indices where the string has a codepoint that doesn't fit in 1 byte
  an Array[Int] of indices where the string has a codepoint of 2 bytes
  an Array[Int] of indices where the string has a codepoint of 3 bytes
  an Array[Int] of indices where the string has a codepoint of 4 bytes
  case class Text(bytes: Array[Byte], multibytePositions: Array[Array[Int]])

  def fromString(s: String): Buffer[String,String] = new Buffer[String,String] {
    val buf = new java.lang.StringBuilder(s)
    def size = buf.size
    def :+(s: String) = { buf.append(s); this }
    def apply(i: Int) = buf.codePointAt(i)
  }
  */

  abstract class NewArray[A] { def apply(size: Int): Array[A] }

  object NewArray extends LowPriorityNewArrays {
    implicit val Byte = new NewArray[Byte] { def apply(size: Int) = new Array[Byte](size) }
    implicit val Double = new NewArray[Double] { def apply(size: Int) = new Array[Double](size) }
    implicit val Long = new NewArray[Long] { def apply(size: Int) = new Array[Long](size) }
    implicit val Int = new NewArray[Int] { def apply(size: Int) = new Array[Int](size) }
  }

  trait LowPriorityNewArrays {
    val AnyRef = new NewArray[AnyRef] { def apply(size: Int) = new Array[AnyRef](size) }
    implicit def Polymorphic[A]: NewArray[A] = AnyRef.asInstanceOf[NewArray[A]]
  }
}
