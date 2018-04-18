package org.unisonweb

import org.unisonweb.util.{Traverse,Monoid}
import ABT.{Abs, AnnotatedTerm, Tm}
import java.lang.Double.{doubleToRawLongBits}

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
      case _ => t
    }
    case _ => t
  }

  /** Convert the term to A-normal form: https://en.wikipedia.org/wiki/A-normal_form. */
  def ANF(t: Term): Term = t match {
    case t @ ABT.Var(_) => t
    // arg1 -> foo (x + 1) blah
    // arg1 -> let arg1 = x + 1; foo arg1 blah
    case ABT.Abs(name, body) => ABT.Abs(name, ANF(body))
    case Apply(f @ (ABT.Var(_) | Lam(_,_) | Id(_)), args) =>
      val (bindings2, args2) =
        args.zipWithIndex.foldRight((List.empty[(Name,Term)], List.empty[Term])) { (argi, accs) =>
          val (bindings, args) = accs
          val (arg, i) = argi
          arg match {
            case Unboxed(_,_) => (bindings, arg :: args)
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
    case ABT.Tm(other) => ABT.Tm(F.instance.map(other)(ANF))
  }

  /** Return a `Term` without an outer `Compiled` constructor. */
  def stripOuterCompiled(t: Term): Term = t match {
    case Compiled(v: Value) => v.decompile
    case Compiled(r: Ref) => r.value.decompile
    case _ => t
  }

  /** Converts self refs within lambdas to regular named let recs. */
  def selfToLetRec(t: Term): Term = {
    assert(Term.freeVars(t).isEmpty)
    val t2 = t.rewriteDown {
      case Term.Self(name) => Term.Var(name)
      case t => t
    }.annotateFree.rewriteUp {
      case t@Lam(names, body) => Term.freeVars(t).toList match {
        case Nil => t
        case rec :: Nil => LetRec(rec -> t)(t)
        case recs => t // this can happen if `t` has already been fullyDecompile'd
      }
      case t => t
    }.annotateFree
    assert (Term.freeVars(t2).isEmpty)
    t2
  }

  /**
   * Removes all `Compiled` nodes from `t` by expanding their definitions and
   * converting cyclic references to `let rec` declarations.
   */
  def fullyDecompile2(t: Term): Term = {
    // 1. Collect full set of refs via transitive closure - a `Set[Ref]`
    // 2. Compute all used names (including self recursive names), freshen names for each `Ref`
    // 3. Introduce one outer let rec block for each `Ref`, substitute away all refs
    // 4. Pass to convert self calls to regular let rec
    // 5. (optional) Pass to float bindings in to their LCA of all reference points,
    //               but there's not much advantage to this.

    def names(t: Term): Set[Name] = t.foldMap[Set[Name]] {
      case Var(name) => Set(name)
      case Self(name) => Set(name)
      case Abs(name, _) => Set(name)
      case _ => Set.empty
    } (Monoid.Set)

    def transitiveClosure(seen: Map[Ref,Term], cur: Term): Map[Ref,Term] = cur match {
      case Id(_) | Unboxed(_,_) | Var(_) | Text(_) => seen
      case Apply(f, args) =>
        args.foldLeft(transitiveClosure(seen, f))(transitiveClosure _)
      case If(cond, t, f) =>
        List(t, f).foldLeft(transitiveClosure(seen, cond))(transitiveClosure _)
      case Lam1(name, body) => transitiveClosure(seen, body)
      case Let1(name, binding, body) =>
        transitiveClosure(transitiveClosure(seen, binding), body)
      case LetRec(bindings, body) =>
        bindings.map(_._2).foldLeft(transitiveClosure(seen, body))(transitiveClosure)
      case Compiled(r : Ref) =>
        if (seen.contains(r)) seen
        else { val v = r.value.decompile; transitiveClosure(seen + (r -> v), v) }
      case Compiled(v) => transitiveClosure(seen, v.toValue.decompile)
    }

    // 1. Collect full set of refs via transitive closure - a `Set[Ref]`
    val refs = transitiveClosure(Map.empty, t)
    val usedNames = refs.values.map(names).foldLeft(names(t))(_ union _)
    // 2. Freshen names for each `Ref`, if needed
    val freshRefNames = refs.keys.view.map(r => (r, ABT.freshen(r.name, usedNames))).toMap
    def replaceRefs(t: Term): Term = selfToLetRec(t).rewriteDown {
      case Compiled(c) => c match {
        case r : Ref => Var(freshRefNames(r))
        case Value.Unboxed(d,typ) => Term.Unboxed(d,typ)
        case v : Value => replaceRefs(v.decompile)
      }
      case t => t
    }
    val refBindings = refs.toList.map { case (ref,tm) => (freshRefNames(ref), replaceRefs(tm)) }
    // 3. Introduce one outer let rec block with all uniquely named refs
    // 4. Pass to convert self calls to regular let rec
    LetRec(refBindings: _*)(replaceRefs(t))
  }

  // todo - test when function being applied is a compound expression

  case class MatchCase[R](pattern: Pattern, guard: Option[R], body: R) {
    assert {
      try {
        body.asInstanceOf[Term.Term] match {
          case ABT.AbsChain(ns,_) => ns.length == pattern.arity
          case _ => pattern.arity == 0
        }
      } catch {
        case _: ClassCastException => true
      }
    }

    def map[R2](f: R => R2): MatchCase[R2] =
      MatchCase(pattern, guard.map(f), f(body))
  }

  object MatchCase {
    def apply[R](pattern: Pattern, body: R): MatchCase[R] =
      MatchCase(pattern, None, body)
  }

  sealed abstract class F[+R]

  object F {

    case class Lam_[R](body: R) extends F[R]
    case class Id_(id: Id) extends F[Nothing]
    case class Apply_[R](fn: R, args: List[R]) extends F[R]
    case class Unboxed_(value: U, typ: UnboxedType) extends F[Nothing]
    case class Text_(txt: util.Text.Text) extends F[Nothing]
    case class LetRec_[R](bindings: List[R], body: R) extends F[R]
    case class Let_[R](binding: R, body: R) extends F[R]
    case class Rec_[R](r: R) extends F[R]
    case class Self_(name: Name) extends F[Nothing]
    case class If_[R](condition: R, ifNonzero: R, ifZero: R) extends F[R]
    case class Match_[R](scrutinee: R, cases: List[MatchCase[R]]) extends F[R]
    case class Compiled_(value: Param) extends F[Nothing]
    // request : <f> a -> {f} a
    case class Request_ [R](id: Id, ctor: ConstructorId, args: List[R]) extends F[R]
    // handle : (forall x . <f> x -> r) -> {f} x -> r
    case class Handle_[R](handler: R, block: R) extends F[R]

    implicit val instance: Traverse[F] = new Traverse[F] {
      override def map[A,B](fa: F[A])(f: A => B): F[B] = fa match {
        case fa @ (Id_(_) | Unboxed_(_,_) | Compiled_(_) | Self_(_) | Text_(_)) =>
          fa.asInstanceOf[F[B]]
        case Lam_(a) => Lam_(f(a))
        case Apply_(fn, args) =>
          val fn2 = f(fn); val args2 = args map f
          Apply_(fn2, args2)
        case LetRec_(bs, body) =>
          val bs2 = bs map f; val body2 = f(body)
          LetRec_(bs2, body2)
        case Let_(b, body) =>
          val b2 = f(b); val body2 = f(body)
          Let_(b2, body2)
        case Rec_(a) => Rec_(f(a))
        case If_(c,a,b) =>
          val c2 = f(c); val a2 = f(a); val b2 = f(b)
          If_(c2, a2, b2)
        case Match_(s, cs) =>
          val s2 = f(s); val cs2 = cs.map(_.map(f))
          Match_(s2, cs2)
        case Handle_(h,b) =>
          val h2 = f(h); val b2 = f(b)
          Handle_(h2, b2)
        case Request_(id, ctor, args) =>
          Request_(id, ctor, args.map(f))
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
  object Match {
    def unapply[A](t: AnnotatedTerm[F,A]): Option[(AnnotatedTerm[F,A], List[MatchCase[AnnotatedTerm[F,A]]])] = t match {
      case Tm(Match_(scrutinee, cases)) => Some((scrutinee, cases))
      case _ => None
    }
    def apply(scrutinee: Term)(cases: MatchCase[Term]*): Term =
      Tm(Match_(scrutinee, cases.toList))
  }

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

  object Id {
    def apply(id: org.unisonweb.Id): Term = Tm(Id_(id))
    def apply(n: Name): Term = Tm(Id_(org.unisonweb.Id(n)))
    def apply(h: Hash): Term = Tm(Id_(org.unisonweb.Id(h)))
    def unapply[A](t: AnnotatedTerm[F,A]): Option[Id] = t match {
      case Tm(Id_(id)) => Some(id)
      case _ => None
    }
  }

  /** A reference to the currently executing lambda. */
  object Self {
    def apply(n: Name): Term = Tm(Self_(n))
    def unapply[A](t: AnnotatedTerm[F,A]): Option[Name] = t match {
      case Tm(Self_(n)) => Some(n)
      case _ => None
    }
  }

  object Unboxed {
    def apply(n: U, typ: UnboxedType): Term = Tm(Unboxed_(n, typ))
    def unapply[A](t: AnnotatedTerm[F,A]): Option[(U, UnboxedType)] =
      t match {
        case Tm(Unboxed_(n, typ)) => Some((n, typ))
        case _ => None
      }
  }

  object Text {
    def apply(txt: util.Text.Text): Term = Tm(Text_(txt))
    def unapply[A](t: AnnotatedTerm[F,A]): Option[util.Text.Text] =
      t match {
        case Tm(Text_(txt)) => Some(txt)
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

  object Request {
    def unapply[A](t: AnnotatedTerm[F,A]): Option[AnnotatedTerm[F,A]] = t match {
      case Tm(Request_(_,_,_)) => Some(t)
      case _ => None
    }
    def apply(id: Id, ctor: ConstructorId, args: List[Term]): Term =
      Tm(Request_(id, ctor, args))
  }
  object Handle {
    def unapply[A](t: AnnotatedTerm[F,A]): Option[(AnnotatedTerm[F,A],AnnotatedTerm[F,A])] = t match {
      case Tm(Handle_(handler, block)) => Some(handler -> block)
      case _ => None
    }
    def apply(handler: Term)(block: Term): Term = Tm(Handle_(handler, block))
  }

  object LetRec {
    def apply(bs: (Name, Term)*)(body: Term): Term =
      if (bs.isEmpty) body
      else Tm(Rec_(bs.map(_._1).foldRight(Tm(LetRec_(bs.map(_._2).toList, body)))((name,body) => Abs(name,body))))
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
  object If {
    def apply(cond: Term, isNot0: Term, is0: Term): Term =
      Tm(If_(cond, isNot0, is0))
    def unapply[A](t: AnnotatedTerm[F,A]): Option[(AnnotatedTerm[F,A],AnnotatedTerm[F,A],AnnotatedTerm[F,A])] = t match {
      case Tm(If_(cond, t, f)) => Some((cond, t, f))
      case _ => None
    }
  }
  object Compiled {
    def apply(v: Param): Term =
      Tm(Compiled_(v))
    def unapply[A](t: AnnotatedTerm[F,A]): Option[Param] = t match {
      case Tm(Compiled_(p)) => Some(p)
      case _ => None
    }
  }

  implicit class ApplySyntax(val fn: Term) extends AnyVal {
    def apply(args: Term*) = Apply(fn, args: _*)
  }

  implicit def bool(b: Boolean): Term = Unboxed(boolToUnboxed(b), UnboxedType.Boolean)
  implicit def number(n: Long): Term = Unboxed(n, UnboxedType.Integer)
  implicit def number(n: Int): Term = Unboxed(n, UnboxedType.Integer)
  implicit def double(n: Double): Term =
    Unboxed(doubleToRawLongBits(n), UnboxedType.Float)
  implicit def stringAsText(s: String): Term = Text(util.Text.fromString(s))
  implicit def nameAsVar(s: Name): Term = Var(s)
  implicit def symbolAsVar(s: Symbol): Term = Var(s.name)
  implicit def symbolAsName(s: Symbol): Name = s.name
  implicit class symbolSyntax(s: Symbol) {
    def v: Term = Var(s.name)
    def b: Term = Id(s.name)
  }

  implicit def stringKeyToNameTerm[A <% Term](kv: (String, A)): (Name, Term) = (kv._1, kv._2)
  implicit def symbolKeyToNameTerm[A <% Term](kv: (Symbol, A)): (Name, Term) = (kv._1, kv._2)
}
