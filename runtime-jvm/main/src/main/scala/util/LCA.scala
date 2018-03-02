package org.unisonweb.util

/**
 * Simple little monoid for computing lowest common ancestor information, often
 * propogated up a tree.
 *
 * Let U = u1, u2, ... be the set of all LCA values combined to form this `LCA`, then:
 * `union` is the union of u1.union, u2.union ...
 * `lcas` is the intersection of u1.union, u2.union ...
 */
case class LCA[A](union: Set[A], lcas: Set[A]) {
  def combine(a: LCA[A]) = LCA(union ++ a.union, union intersect a.union)
}

object LCA {
  def single[A](a: A): LCA[A] = LCA(Set(a), Set())
  def empty[A]: LCA[A] = LCA(Set(), Set())
}
