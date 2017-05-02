package org.unisonweb

import org.unisonweb.util.{Functor,Traverse}

sealed abstract class ABT[F[+_],+R] {
  import ABT._
  def map[R2](g: R => R2)(implicit F: Functor[F]): ABT[F,R2] = this match {
    case Tm_(f) => Tm_(F.map(f)(g))
    case Abs_(name, body) => Abs_(name, g(body))
    case v@Var_(_) => v
  }
}

object ABT {
  type Name = String
  case class Var_[F[+_]](name: Name) extends ABT[F,Nothing]
  case class Abs_[F[+_],R](name: Name, body: R) extends ABT[F,R]
  case class Tm_[F[+_],R](f: F[R]) extends ABT[F,R]

  case class AnnotatedTerm[F[+_],A](annotation: A, get: ABT[F,AnnotatedTerm[F,A]]) {
    def map[B](f: A => B)(implicit F: Functor[F]): AnnotatedTerm[F,B] =
      AnnotatedTerm(f(annotation), get.map(_.map(f)))
    override def toString = get.toString
  }
  type Term[F[+_]] = AnnotatedTerm[F,Set[Name]]

  def rename[F[+_]](from: Name, to: Name)(self: Term[F])(implicit F: Traverse[F]): Term[F] =
    if (!self.annotation.contains(from)) self
    else self match {
      case Var(n) => if (n == from) Var(to) else self
      case Abs(name, body) => Abs(name, rename(from,to)(body))
      case Tm(f) => Tm(F.map(f)(e => rename(from,to)(e)))
    }

  def subst[F[+_]](original: Name, sub: Term[F])(self: Term[F])(implicit F: Traverse[F]): Term[F] =
    if (!self.annotation.contains(original)) self
    else self match {
      case Var(n) => if (n == original) sub else self
      case Abs(name, body) =>
        if (sub.annotation.contains(name)) {
          val name2 = freshen(name, sub.annotation)
          Abs(name2, subst(original,sub)(rename(name, name2)(body)))
        }
        else Abs(name, subst(original, sub)(body))
      case Tm(f) => Tm(F.map(f)(e => subst(original,sub)(e)))
    }

  def substs[F[+_]](subs: Map[Name, Term[F]])(self: Term[F])(implicit F: Traverse[F]): Term[F] =
    substs(subs, subs.values.foldLeft(Set.empty[Name])(_ union _.annotation))(self)

  def substs[F[+_]](subs: Map[Name, Term[F]], taken: Set[Name])(self: Term[F])(implicit F: Traverse[F]): Term[F] =
    // if none of the freeVars of this subtree have a mapping in subs, can skip whole subtree
    if (!subs.keys.exists(original => self.annotation.contains(original))) self
    else self match {
      case Var(n) => subs.getOrElse(n, self)
      case Abs(name, body) =>
        if (taken.contains(name)) {
          val name2 = freshen(name, taken)
          Abs(name2, substs(subs, taken)(rename(name,name2)(body)))
        }
        else Abs(name, substs(subs, taken)(body))
      case Tm(f) => Tm(F.map(f)(e => substs(subs, taken)(e)))
    }

  object Var {
    def unapply[F[+_]](t: Term[F]) = t.get match {
      case Var_(name) => Some(name)
      case _ => None
    }
    def apply[F[+_]](name: Name): Term[F] = AnnotatedTerm(Set(name), Var_(name))
  }

  object Tm {
    def unapply[F[+_]](t: Term[F]) = t.get match {
      case Tm_(f) => Some(f)
      case _ => None
    }
    def apply[F[+_]](f: F[Term[F]])(implicit F: Traverse[F]): Term[F] = {
      val fvs = F.toVector(f).map(_.annotation).foldLeft(Set.empty[Name])(_ union _)
      AnnotatedTerm(fvs, Tm_(f))
    }
  }

  object Abs {
    def unapply[F[+_]](t: Term[F]): Option[(Name, Term[F])] = t.get match {
      case Abs_(name, body) => Some((name, body))
      case _ => None
    }
    def apply[F[+_]](name: Name, body: Term[F]): Term[F] = {
      AnnotatedTerm(body.annotation - name, Abs_(name, body))
    }
  }

  object AbsChain {
    def unapply[F[+_]](t: Term[F]): Option[(List[Name], Term[F])] = {
      def go(names: List[Name], t: Term[F]): Option[(List[Name], Term[F])] = t match {
        case Abs(name, body) => go(name :: names, body)
        case _ => if (names.isEmpty) None else Some((names.reverse, t))
      }
      go(List(), t)
    }
  }

  def freshen(v: Name, taken: Set[Name]): Name =
    Stream.continually(v).zipWithIndex.map { case (name,i) => name + i }.dropWhile(taken.contains(_)).head
}
