package org.unisonweb

import org.unisonweb.util.{Lazy, Traverse, LCA}
import ABT.{Abs, AnnotatedTerm, Tm}
import compilation.Value
import compilation.Ref

object Term {

  type Name = ABT.Name
  val Name = ABT.Name
  type Term = ABT.Term[F]

  def freeVars(t: Term): Set[Name] = t.annotation
  def freshen(v: Name, t: Term): Name = ABT.freshen(v, freeVars(t))

  def betaReduce(name: Name, body: Term)(arg: Term): Term =
    ABT.subst(name, arg)(body)
  def betaReduce2(name1: Name, name2: Name, body: Term)(arg1: Term, arg2: Term): Term =
    betaReduce(name2, betaReduce(name1, Lam(name2)(body))(arg1))(arg2)
  def betaReduce3(name1: Name, name2: Name, name3: Name, body: Term)(arg1: Term, arg2: Term, arg3: Term): Term =
    betaReduce(name3, betaReduce(name2, betaReduce(name1, Lam(name2,name3)(body))(arg1))(arg2))(arg3)
  def betaReduce4(name1: Name, name2: Name, name3: Name, name4: Name, body: Term)(arg1: Term, arg2: Term, arg3: Term, arg4: Term): Term =
    betaReduce(name4, betaReduce(name3, betaReduce(name2, betaReduce(name1,
    Lam(name2,name3,name4)(body))(arg1))(arg2))(arg3))(arg4)

  def etaNormalForm(t: Term): Term = t match {
    case Lam1(x, Apply(f, args)) => args.lastOption match {
      case None => etaNormalForm(Lam(x)(f))
      case Some(Var(x2)) => if (x == x2) etaNormalForm(Apply(f, args.dropRight(1): _*))
                            else t
    }
    case _ => t
  }

  /** Convert the term to A-normal form: https://en.wikipedia.org/wiki/A-normal_form. */
  def ANF(t: Term): Term = t match {
    case t @ ABT.Var(_) => t
    // arg1 -> foo (x + 1) blah
    // arg1 -> let arg1 = x + 1; foo arg1 blah
    case ABT.Abs(name, body) => ABT.Abs(name, ANF(body))
    case Apply(f @ (ABT.Var(_) | Lam(_,_) | Builtin(_)), args) =>
      val (bindings2, args2) =
        args.zipWithIndex.foldRight((List.empty[(Name,Term)], List.empty[Term])) { (argi, accs) =>
          val (bindings, args) = accs
          val (arg, i) = argi
          arg match {
            case Num(_) => (bindings, arg :: args)
            case ABT.Var(_) => (bindings, arg :: args)
            case lam @ Lam(_, _) /* if freeVars(lam).isEmpty */ => (bindings, arg :: args)
            case arg =>
              val freshName = freshen(Name(s"_arg$i"), arg)
              ((freshName, arg) :: bindings, Var(freshName) :: args)
          }
        }
      Let(bindings2: _*)(Apply(f, args2: _*))
    case Apply(f, args) =>
      val freshName = freshen(Name("_f"), f)
      Let(freshName -> f)(ANF(Apply(Var(freshName), args: _*)))
    case If0(cond @ (ABT.Var(_) | Lam(_,_) | Builtin(_)), if0, ifNot0) =>
      If0(cond, ANF(if0), ANF(ifNot0))
    case If0(cond, if0, ifNot0) =>
      val freshName = freshen(Name("_cond"), cond)
      Let(freshName -> cond)(ANF(If0(Var(freshName), if0, ifNot0)))
    case ABT.Tm(other) => ABT.Tm(F.instance.map(other)(ANF))
  }

  private case class DecompileCycle(refs: Vector[Ref], i: Int) extends Throwable {
    override def toString: String = s"DecompileCycle($refs, $i)"
    // override def fillInStackTrace = this
  }

  def fullyDecompile(t: Term): Term = {
    def annotateRefs(t: Term): AnnotatedTerm[Term.F, (Set[Name], LCA[Ref])] =
      t.annotateUp[LCA[Ref]](_ combine _, LCA.empty) {
        case c@Compiled(r@Ref(_, _)) => c map { _ => LCA.single(r) }
        case x => x map { _ => LCA.empty }
      }

    def transitiveClosure(seen: Set[Ref], cur: Term): Set[Ref] = cur match {
      case Builtin(_) | Num(_) | Var(_) => seen
      case Apply(f, args) =>
        args.foldLeft(transitiveClosure(seen, f))(transitiveClosure _)
      case If0(cond, ifZero, ifNonzero) =>
        List(ifZero, ifNonzero).foldLeft(transitiveClosure(seen, cond))(transitiveClosure _)
      case Lam1(name, body) => transitiveClosure(seen, body)
      case Let1(name, binding, body) =>
        transitiveClosure(transitiveClosure(seen, binding), body)
      case LetRec(bindings, body) =>
        bindings.map(_._2).foldLeft(transitiveClosure(seen, body))(transitiveClosure)
      case Compiled(r@Ref(name, value)) => if (seen.contains(r)) seen else transitiveClosure(seen + r, value.decompile)
      case Compiled(v) => transitiveClosure(seen, v.decompile)
    }

    type TermLR = AnnotatedTerm[Term.F, Set[Ref]]
    // println("input term: " + t)
    // println("annotateRefs: " + annotateRefs(t))

    val letGroups: TermLR = annotateRefs(t match {
      case Compiled(r@Ref(_, _)) => r.value.decompile
      case v => v
    }).annotateDown(Set.empty[Ref]) { (seen, tm) =>
      val lcas2 = tm.annotation._2.lcas -- seen
      val bindingsToIntroduce = lcas2.foldLeft(Set.empty[Ref])((seen,ref) => transitiveClosure(seen, ref.value.decompile))
      (seen ++ bindingsToIntroduce, bindingsToIntroduce)
    }
    // println("let groups: " + letGroups)

    import ABT.{Tm_, Abs_}
    def Tm(f: Term.F[TermLR]): AnnotatedTerm[Term.F, Set[Ref]] = AnnotatedTerm(Set.empty[Ref], Tm_(f))

    val letTree = letGroups rewriteDown { t =>
      if (t.annotation.isEmpty) t
      else {
        def letrec(bs: List[(Name,TermLR)], body: TermLR): TermLR =
          Tm(F.Rec_(bs.map(_._1).foldRight(Tm(F.LetRec_(bs.map(_._2).toList, body)))((name,body) => AnnotatedTerm(Set.empty, Abs_(name,body)))))
        def replaceRefs(refs: Set[Ref], t: Term): Term = t.rewriteDown {
          case c@Compiled(r2@Ref(name2,_)) => if (refs.contains(r2)) Var(name2) else c
          case c => c
        }
        // introduce a let rec
        letrec(
          t.annotation.toList.map(r => r.name -> replaceRefs(t.annotation, r.value.decompile).map(_ => Set.empty[Ref])),
          replaceRefs(t.annotation, t.map(_ => Set.empty)).map(_ => Set.empty)
        )
      }
    }
    letTree.annotateFree
  }

  // todo - test when function being applied is a compound expression

  sealed abstract class F[+R]

  object F {

    case class Lam_[R](body: R) extends F[R]
    case class Builtin_(name: Name) extends F[Nothing]
    case class Apply_[R](fn: R, args: List[R]) extends F[R]
    case class Num_(value: Double) extends F[Nothing]
    case class LetRec_[R](bindings: List[R], body: R) extends F[R]
    case class Let_[R](binding: R, body: R) extends F[R]
    case class Rec_[R](r: R) extends F[R]
    case class If0_[R](condition: R, ifZero: R, ifNonzero: R) extends F[R]
    case class Delayed_(name: Name, delayed: Lazy[Value]) extends F[Nothing]
    case class Compiled_(value: Value) extends F[Nothing]
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
          val fn2 = f(fn); val args2 = args map f
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
        case a@Delayed_(_, _) => a
        case a@Compiled_(_) => a
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
    def unapply[A](t: AnnotatedTerm[F,A]): Option[(Name,AnnotatedTerm[F,A])] = t match {
      case Tm(Lam_(ABT.Abs(name, body))) => Some((name, body))
      case _ => None
    }
    def apply(name: Name)(body: Term): Term =
      Tm(Lam_(Abs(name, body)))
  }

  object Lam {
    def apply(names: Name*)(body: Term): Term =
      names.foldRight(body)((name,body) => Lam1(name)(body))

    // todo: produce an Array[Name]
    def unapply[A](t: AnnotatedTerm[F,A]): Option[(List[Name], AnnotatedTerm[F,A])] = {
      def go(acc: List[Name], t: AnnotatedTerm[F,A]): Option[(List[Name], AnnotatedTerm[F,A])] = t match {
        case Lam1(n, t) => go(n :: acc, t)
        case _ if acc.isEmpty => None
        case _ => Some(acc.reverse -> t)
      }
      go(List(), t)
    }
  }

  object Builtin {
    def apply(n: Name): Term = Tm(Builtin_(n))
    def unapply[A](t: AnnotatedTerm[F,A]): Option[Name] = t match {
      case Tm(Builtin_(n)) => Some(n)
      case _ => None
    }
  }

  object Num {
    def apply(n: Double): Term = Tm(Num_(n))
    def unapply[A](t: AnnotatedTerm[F,A]): Option[Double] = t match {
      case Tm(Num_(n)) => Some(n)
      case _ => None
    }
  }

  object Apply {
    def unapply[A](t: AnnotatedTerm[F,A]): Option[(AnnotatedTerm[F,A], List[AnnotatedTerm[F,A]])] = t match {
      case Tm(Apply_(f, args)) => Some((f, args))
      case _ => None
    }
    def apply(f: Term, args: Term*): Term =
      if (args.isEmpty) f
      else Tm(Apply_(f, args.toList))
  }

  object Var {
    def apply(n: Name): Term = ABT.Var(n)
    def unapply[A](t: AnnotatedTerm[F,A]): Option[Name] = ABT.Var.unapply(t)
  }

  object Yield {
    def unapply[A](t: AnnotatedTerm[F,A]): Option[AnnotatedTerm[F,A]] = t match {
      case Tm(Yield_(t)) => Some(t)
      case _ => None
    }
    def apply(t: Term): Term = Tm(Yield_(t))
  }
  object Handle {
    def unapply[A](t: AnnotatedTerm[F,A]): Option[(AnnotatedTerm[F,A],AnnotatedTerm[F,A])] = t match {
      case Tm(Handle_(handler, block)) => Some(handler -> block)
      case _ => None
    }
    def apply(handler: Term, block: Term): Term = Tm(Handle_(handler, block))
  }

  object LetRec {
    def apply(bs: (Name, Term)*)(body: Term): Term =
      Tm(Rec_(bs.map(_._1).foldRight(Tm(LetRec_(bs.map(_._2).toList, body)))((name,body) => Abs(name,body))))
    def unapply[A](t: AnnotatedTerm[F,A]): Option[(List[(Name,AnnotatedTerm[F,A])], AnnotatedTerm[F,A])] = t match {
      case Tm(Rec_(ABT.AbsChain(names, Tm(LetRec_(bindings, body))))) =>
        Some((names zip bindings, body))
      case _ => None
    }
  }

  object Let1 {
    def apply(name: Name, binding: Term)(body: Term): Term =
      Tm(Let_(binding, Abs(name, body)))
    def unapply[A](t: AnnotatedTerm[F,A]): Option[(Name,AnnotatedTerm[F,A],AnnotatedTerm[F,A])] = t match {
      case Tm(Let_(binding, Abs(name, body))) => Some((name,binding,body))
      case _ => None
    }
  }
  object Let {
    def apply(bs: (Name,Term)*)(body: Term): Term =
      bs.foldRight(body)((b,body) => Let1(b._1, b._2)(body))
    def unapply[A](t: AnnotatedTerm[F,A]): Option[(List[(Name,AnnotatedTerm[F,A])], AnnotatedTerm[F,A])] = {
      def go(bs: List[(Name,AnnotatedTerm[F,A])], t: AnnotatedTerm[F,A])
        : Option[(List[(Name,AnnotatedTerm[F,A])], AnnotatedTerm[F,A])] = t match {
          case Tm(Let_(binding, Abs(name, body))) => go((name,binding) :: bs, body)
          case _ => if (bs.isEmpty) None else Some((bs.reverse, t))
        }
      go(List(), t)
    }
  }
  object If0 {
    def apply(cond: Term, is0: Term, not0: Term): Term =
      Tm(If0_(cond, is0, not0))
    def unapply[A](t: AnnotatedTerm[F,A]): Option[(AnnotatedTerm[F,A],AnnotatedTerm[F,A],AnnotatedTerm[F,A])] = t match {
      case Tm(If0_(cond, t, f)) => Some((cond, t, f))
      case _ => None
    }
  }
  object Compiled {
    def apply(v: Value): Term =
      Tm(Compiled_(v))
    def unapply[A](t: AnnotatedTerm[F,A]): Option[Value] = t match {
      case Tm(Compiled_(v)) => Some(v)
      case _ => None
    }
  }

  implicit class ApplySyntax(val fn: Term) extends AnyVal {
    def apply(args: Term*) = Apply(fn, args: _*)
  }

  implicit def number(n: Double): Term = Num(n)
  implicit def number(n: Int): Term = Num(n)
  implicit def stringAsVar(s: Name): Term = Var(s)
  implicit def symbolAsVar(s: Symbol): Term = Var(s.name)
  implicit class symbolSyntax(s: Symbol) {
    def v: Term = Var(s.name)
  }

  implicit def stringKeyToNameTerm[A <% Term](kv: (String, A)): (Name, Term) = (kv._1, kv._2)
  implicit def symbolKeyToNameTerm[A <% Term](kv: (Symbol, A)): (Name, Term) = (kv._1, kv._2)
}
