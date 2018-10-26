package org.unisonweb

import util.Sink
import org.unisonweb.Term._
import ABT.{AnnotatedTerm}

object Serialization {

  type Term = org.unisonweb.ABT.Term[F]

  object V0 {
    /**
     * Serializes a Unison term. Expects the term to be annotated with its
     * free variables.
     */
    def putTerm(sink: Sink, term: Term): Unit = {
      import sink._
      val freeVars = term.annotation.toList

      def go(t: AnnotatedTerm[Term.F,(Set[Name], Vector[Name])]): Unit = {
        t match {
          case ABT.Var(name) =>
            putByte(0)
            putVarRef(t.annotation._2, name)
          case ABT.Tm(f) =>
            putByte(1)
            putF(t.map(_._1))
          case ABT.Abs(name, body) =>
            putByte(2)
            putVar(name)
            go(body)
        }
      }

      def putVar(v: Name) = {
        putVarLong(0)
        putString(v.toString)
      }

      def putVarRef(env: Vector[Name], v: Name) =
        env indexOf v match {
          case -1 => freeVars indexOf v match {
            case -1 => sys.error("impossible: var not free or bound")
            case i =>
              putByte(1)
              putVarLong(i)
          }
          case i =>
            putByte(0)
            putVarLong(i)
        }

      def putF(f: Term): Unit =
        f match {
          case Unboxed(u, t) =>
            t match {
              case UnboxedType.Int =>
                putByte(0)
                putLong(unboxedToLong(u))
              case UnboxedType.Nat =>
                putByte(1)
                putLong(u)
              case UnboxedType.Float =>
                putByte(2)
                putDouble(unboxedToDouble(u))
              case UnboxedType.Boolean =>
                putByte(3)
                putBoolean(unboxedToBool(u))
            }
          case Text(txt) =>
            putByte(4)
            putString(util.Text toString txt)
          case Term.Id(r) =>
            putByte(5)
            putReference(r)
          case Constructor(id, ConstructorId(ctor)) =>
            putByte(6)
            putReference(id)
            putVarLong(ctor)
          case Request(id, ConstructorId(ctor)) =>
            putByte(7)
            putReference(id)
            putVarLong(ctor)
          case Handle(h, a) =>
            putByte(8)
            putTerm(sink, h)
            putTerm(sink, a)
          case Apply(f, Nil) => ()
          case Apply(f, a::Nil) =>
            putByte(9)
            putTerm(sink, f)
            putTerm(sink, a)
          case Apply(f, args) =>
            // Expand out applications, as V0 expects them one at a time
            putF(args.foldLeft(f)((t,a) => Apply(t, a)))
          // Type annotations are byte 10, but we don't have any
          case Sequence(seq) =>
            putByte(11)
            putFramedSeq(seq.toList)(putTerm)
          case If(cond, t, f) =>
            putByte(12)
            putTerm(sink, cond)
            putTerm(sink, t)
            putTerm(sink, f)
          case And(x,y) =>
            putByte(13)
            putTerm(sink, x)
            putTerm(sink, y)
          case Or(x,y) =>
            putByte(14)
            putTerm(sink, x)
            putTerm(sink, y)
          case Lam(_, body) =>
            putByte(15)
            putTerm(sink, body)
          case LetRec(bindings, body) =>
            // Put a Cycle
            putByte(3)
            putLet(16, bindings, body)
          case Let(bindings, body) =>
            putLet(17, bindings, body)
          case Match(scrutinee, cases) =>
            putByte(18)
            putTerm(sink, scrutinee)
            putFramedSeq1(cases)(putMatchCase _)
        }

      def putMatchCase(c: MatchCase[Term]): Unit = {
        val MatchCase(pattern, guard, body) = c
        putPattern(pattern)
        putOption1(guard)(putTerm(sink,_))
        putTerm(sink, body)
      }

      def putPattern(pattern: Pattern): Unit = pattern match {
        case Pattern.Uncaptured => putByte(0)
        case Pattern.Wildcard => putByte(1)
        case Pattern.LiteralU(u, t) => t match {
          case UnboxedType.Boolean =>
            putByte(2)
            putBoolean(unboxedToBool(u))
          case UnboxedType.Int =>
            putByte(3)
            putLong(unboxedToLong(u))
          case UnboxedType.Nat =>
            putByte(4)
            putLong(u)
          case UnboxedType.Float =>
            putByte(5)
            putDouble(unboxedToDouble(u))
        }
        case Pattern.Data(id, cid, patterns) =>
          putByte(6)
          putReference(id)
          putVarLong(cid.toInt)
          putFramedSeq1(patterns)(putPattern)
        case Pattern.As(p) =>
          putByte(7)
          putPattern(p)
        case Pattern.EffectPure(p) =>
          putByte(8)
          putPattern(p)
        case Pattern.EffectBind(id, cid, args, k) =>
          putByte(9)
          putReference(id)
          putVarLong(cid.toInt)
          putFramedSeq1(args)(putPattern)
          putPattern(k)
      }

      def putLet(b: Byte,
                 bindings: List[(Name, AnnotatedTerm[F,Set[Name]])],
                 body: AnnotatedTerm[F,Set[Name]]): Unit = {
        bindings.foreach {
          // Abs all the bound variables
          case (name, binding) =>
            putByte(2)
            putVar(name)
        }
        // Finally put the letrec term
        putByte(1)
        putByte(b)
        putFramedSeq(bindings) {
          case (s, (_, binding)) => putTerm(s, binding)
        }
        putTerm(sink, body)
      }

      def putReference(id: Id): Unit = id match {
        case Id.Builtin(Name(name)) =>
          putByte(0)
          putString(name)
        case Id.HashRef(Hash(bytes)) =>
          putByte(1)
          put(bytes)
      }
      putFramedSeq1(freeVars)(putVar _)
      go(ABT.annotateBound(term))
    }


  }
}
