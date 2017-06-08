package org.unisonweb.util

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]
}
