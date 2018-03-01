package org.unisonweb.util

class Lazy[A](a: => A) {
  private var _evaluated = false

  def evaluated: Boolean = _evaluated

  lazy val value: A = { _evaluated = true; a }

  override def toString = if (_evaluated) s"Lazy($a)" else "Lazy(...)"
}
object Lazy {
  def apply[A](a: => A) = new Lazy(a)
}
