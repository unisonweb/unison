package org.unisonweb.util

class NonEmptyList[A](val head: A, tail: List[A]) {
  def <::(a: A) = new NonEmptyList[A](a, head :: tail)
  def map[B](f: A => B) = new NonEmptyList[B](f(head), tail.map(f))
}
