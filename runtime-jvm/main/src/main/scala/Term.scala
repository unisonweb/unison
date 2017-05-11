package org.unisonweb

import org.unisonweb.util.Traverse
import ABT.{Tm,Abs}

object Term {

  type Name = String
  type Term = ABT.Term[F]

  def freeVars(t: Term): Set[Name] = t.annotation
  def freshen(v: Name, t: Term): Name = ABT.freshen(v, freeVars(t))

  sealed abstract class F[+R]

  object F {

    case class Lam_[R](body: R) extends F[R]
    case class Builtin_(name: String) extends F[Nothing]
    case class Apply_[R](f: R, args: List[R]) extends F[R]
    case class Num_(value: Double) extends F[Nothing]
    case class LetRec_[R](bindings: List[R], body: R) extends F[R]
    case class Let_[R](binding: R, body: R) extends F[R]
    case class Rec_[R](r: R) extends F[R]
    case class If0_[R](condition: R, ifTrue: R, ifFalse: R) extends F[R]
    // yield : f a -> a|f
    case class Yield_[R](effect: R) extends F[R]
    // handle : (forall x . f x -> (x -> y|f+g) -> y|f+g) -> a|f+g -> a|g
    case class Handle_[R](handler: R, block: R) extends F[R]

    // todo pattern matching
    // todo data constructors

    implicit val instance: Traverse[F] = new Traverse[F] {
      override def map[A,B](fa: F[A])(f: A => B): F[B] = fa match {
        case Lam_(a) => Lam_(f(a))
        case b@Builtin_(_) => b
        case Apply_(fn, args) =>
          val fn2 = f(fn); val args2 = args map f;
          Apply_(fn2, args2)
        case n@Num_(_) => n
        case LetRec_(bs, body) =>
          val bs2 = bs map f; val body2 = f(body)
          LetRec_(bs2, body2)
        case Let_(b, body) =>
          val b2 = f(b); val body2 = f(body)
          Let_(b2, body2)
        case Rec_(a) => Rec_(f(a))
        case If0_(c,a,b) =>
          val c2 = f(c); val a2 = f(a); val b2 = f(b)
          If0_(c2, a2, b2)
        case Handle_(h,b) =>
          val h2 = f(h); val b2 = f(b)
          Handle_(h2, b2)
        case Yield_(e) =>
          val e2 = f(e)
          Yield_(e2)
      }
      def mapAccumulate[S,A,B](fa: F[A], s0: S)(g: (A,S) => (B,S)): (F[B], S) = {
        var s = s0
        val fb = map(fa) { a => val (b, s2) = g(a, s); s = s2; b }
        (fb, s)
      }
    }
  }

  import F._

  // smart patterns and constructors
  object Lam1 {
    def unapply(t: Term): Option[(Name,Term)] = t match {
      case Tm(Lam_(ABT.Abs(name, body))) => Some((name, body))
      case _ => None
    }
    def apply(name: Name)(body: Term): Term =
      Tm(Lam_(Abs(name, body)))
  }

  object Lam {
    def apply(names: Name*)(body: Term): Term =
      names.foldRight(body)((name,body) => Lam1(name)(body))

    def unapply(t: Term): Option[(List[Name], Term)] = {
      def go(acc: List[Name], t: Term): Option[(List[Name], Term)] = t match {
        case Lam1(n, t) => go(n :: acc, t)
        case _ if acc.isEmpty => None
        case _ => Some(acc.reverse -> t)
      }
      go(List(), t)
    }
  }

  object Builtin {
    def apply(n: Name): Term = Tm(Builtin_(n))
    def unapply(t: Term): Option[Name] = t match {
      case Tm(Builtin_(n)) => Some(n)
      case _ => None
    }
  }

  object Num {
    def apply(n: Double): Term = Tm(Num_(n))
    def unapply(t: Term): Option[Double] = t match {
      case Tm(Num_(n)) => Some(n)
      case _ => None
    }
  }

  object Apply {
    def unapply(t: Term): Option[(Term, List[Term])] = t match {
      case Tm(Apply_(f, args)) => Some((f, args))
      case _ => None
    }
    def apply(f: Term, args: Term*): Term =
      Tm(Apply_(f, args.toList))
  }

  object Var {
    def apply(n: Name): Term = ABT.Var(n)
    def unapply(t: Term) = ABT.Var.unapply(t)
  }

  object Yield {
    def unapply(t: Term): Option[Term] = t match {
      case Tm(Yield_(t)) => Some(t)
      case _ => None
    }
    def apply(t: Term): Term = Tm(Yield_(t))
  }
  object Handle {
    def unapply(t: Term): Option[(Term,Term)] = t match {
      case Tm(Handle_(handler, block)) => Some(handler -> block)
      case _ => None
    }
    def apply(handler: Term, block: Term): Term = Tm(Handle_(handler, block))
  }

  object LetRec {
    def apply(bs: (Name, Term)*)(body: Term): Term =
      Tm(Rec_(bs.map(_._1).foldRight(Tm(LetRec_(bs.map(_._2).toList, body)))((name,body) => Abs(name,body))))
    def unapply(t: Term): Option[(List[(Name,Term)], Term)] = t match {
      case Tm(Rec_(ABT.AbsChain(names, Tm(LetRec_(bindings, body))))) =>
        Some((names zip bindings, body))
      case _ => None
    }
  }

  object Let1 {
    def apply(name: Name, binding: Term)(body: Term): Term =
      Tm(Let_(binding, Abs(name, body)))
  }
  object Let {
    def apply(bs: (Name,Term)*)(body: Term): Term =
      bs.foldRight(body)((b,body) => Let1(b._1, b._2)(body))
    def unapply(t: Term): Option[(List[(Name,Term)], Term)] = {
      def go(bs: List[(Name,Term)], t: Term): Option[(List[(Name,Term)], Term)] = t match {
        case Tm(Let_(binding, Abs(name, body))) => go((name,binding) :: bs, body)
        case _ => if (bs.isEmpty) None else Some((bs.reverse, t))
      }
      go(List(), t)
    }
  }
  object If0 {
    def apply(cond: Term, is0: Term, not0: Term): Term =
      Tm(If0_(cond, is0, not0))
    def unapply(t: Term): Option[(Term,Term,Term)] = t match {
      case Tm(If0_(cond, t, f)) => Some((cond, t, f))
    }
  }

  implicit def number(n: Double): Term = Num(n)

  implicit class ApplySyntax(val fn: Term) extends AnyVal {
    def apply(args: Term*) = Apply(fn, args: _*)
  }
}
