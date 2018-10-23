package org.unisonweb

object Serialization {

  object V0 {
    def putABT[F[_],R](sink: Sink,
                       term: ABT[F,R],
                       putVar: ABT.Name => Unit,
                       putR: R => Unit,
                       putF: Forall[λ[A => ((A => Unit), F[A]) => Unit]]
                      ): Unit =
      term.freeVars

    def putTerm(sink: Sink, term: Term): Unit = {
      import sink._
      term match {
        case Unboxed(u, t) =>
          t match {
            case Int =>
              putByte(0)
              putLong(u)
            case Nat =>
              putByte(1)
              putLong(u)
            case Float =>
              putByte(2)
              putDouble(unboxedToDouble(u))
            case Boolean =>
              putByte(3)
              putBoolean(unboxedToBool(u))
          }
        case Text(txt) =>
          putByte(4)
          putString(txt)
        case Id(r) =>
          putByte(5)
          putReference(sink, r)
        case Constructor(id, ConstructorId(ctor)) =>
          putByte(6)
          putReference(sink, id)
          putVarLong(ctor)
        case Request(id, ConstructorId(ctor)) =>
          putByte(7)
          putReference(sink, id)
          putVarLong(ctor)
        case Handle(h, a) =>
          putByte(8)
          putTerm(sink, h)
          putTerm(sink, a)
        case Apply(f, arg) =>
          putByte(9)
          putTerm(sink, f)
          putTerm(sink, arg)
        // Type annotations are byte 10, but we don't have any
        case Sequence(seq) =>
          putByte(11)
          putFramedSeq(seq)(putTerm)
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
          putLet(s, 16, bindings, body)
        case Let(bindings, body) =>
          putLet(s, 17, bindings, body)
        case Match(scrutinee, cases) =>
          putByte(18)
          putTerm(sink, scrutinee)
          putFramedSeq(cases)(putMatchCase(_, _))
      }
    }

    def putLet(sink: Sink,
               b: Byte,
               bindings: List[(Name, AnnotatedTerm[F,A])],
               body: AnnotatedTerm[F,A]): Unit = {
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

    def putReference(sink: Sink, id: Id): Unit = id match {
      case Builtin(Name(name)) =>
        putByte(0)
        putString(name)
      case HashRef(Hash(bytes)) =>
        putByte(1)
        put(bytes)
    }
  }

}
