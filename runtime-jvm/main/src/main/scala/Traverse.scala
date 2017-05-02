package org.unisonweb.util

trait Traverse[F[_]] extends Functor[F] {

  /** Inefficient but correct implementation of `toVector` in terms of `mapAccumulate`. */
  def toVector[A](f: F[A]): Vector[A] = mapAccumulate(f, Vector.empty[A])((a, buf) => (a, buf :+ a))._2

  /** The only function that must be implemented. `map` implemented in terms of `mapAccumulate`
   *  must be consistent with the functorial `map`:
   *    `mapAccumulate(fs, ())((a,_) => (g(a), ())) == map(g)(fs)`
   */
  def mapAccumulate[S,A,B](f: F[A], s0: S)(g: (A,S) => (B,S)): (F[B], S)

  def sequence[G[_],A](f: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = {
    val fgs: G[Vector[A]] = balancedSequence(toVector(f))
    G.map(fgs) { (labels: Vector[A]) =>
      mapAccumulate(f, labels)((_, labels) => (labels.head, labels.tail))._1 // a bit hacky, but safe assuming toList is lawful
      // NB: in a DT language or maybe via path dependent types, probably some way to have typechecker verify safety of this
    }
  }

  private def balancedSequence[G[_],A](v: Vector[G[A]])(implicit G: Applicative[G]): G[Vector[A]] =
    if (v.isEmpty) G.pure(Vector.empty[A])
    else {
      val (l,r) = v.splitAt((v.length / 2).toInt)
      val l2 = balancedSequence(l)
      val r2 = balancedSequence(r)
      G.map2(l2, r2)(_ ++ _)
    }
}
