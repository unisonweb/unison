package org.unisonweb.util

/**
 * Algebraic structure over some type, `A`, satisfying:
 *  - op(id, x) == x` (left identity)
 *  - op(x, id) == x` (right identity)
 *  - op(op(x, y), z) == op(x, op(y, z))` (associativity).
 */
trait Monoid[A] {
  def id: A
  def op(a1: A, a2: A): A

  /**
   * Allow the monoid to choose associativity when reducing a sequence,
   * as some monoids (like List with `++`) perform best right associative,
   * others (like `Array` with ++) perform best as a balanced reduction,
   * etc.
   */
  def reduce(s: Seq[A]): A = s.foldLeft(id)(op)
}

object Monoid {

  implicit def Set[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    type T = Set[A]
    def id = collection.immutable.Set.empty[A]
    def op(a: T, a2: T) = a union a2
  }

  implicit def product[A,B](implicit A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
    type T = (A,B)
    val id = (A.id, B.id)
    def op(a: T, b: T) = (A.op(a._1, b._1), B.op(a._2, b._2))

    override def reduce(s: Seq[T]): T =
      A.reduce(s.view.map(_._1)) -> B.reduce(s.view.map(_._2))
  }

  implicit val unit: Monoid[Unit] = new Monoid[Unit] {
    type T = Unit
    def id = ()
    def op(a: T, b: T) = ()
  }
}
