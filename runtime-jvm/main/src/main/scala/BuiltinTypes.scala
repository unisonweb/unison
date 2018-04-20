package org.unisonweb

import Term.{Term,Name}
import Term.Syntax._
import compilation.Computation

object BuiltinTypes {

  object Unit {
    val Id = org.unisonweb.Id("Unit")
    val pattern: Pattern = Pattern.Data(Id, ConstructorId(0), Nil)
    val term: Term = Term.Constructor(Id, ConstructorId(0))
    val value: Value = Value.Data(Id, ConstructorId(0), Array())
  }

  object Tuple {
    val Id = org.unisonweb.Id("Tuple")
    def pattern(ps: Pattern*): Pattern =
      ps.foldRight(Unit.pattern)((hd,tl) => Pattern.Data(Id, ConstructorId(0), List(hd,tl)))
    def term(ts: Term*): Term =
      ts.foldRight(Unit.term)((hd,tl) => Term.Constructor(Id, ConstructorId(0))(hd,tl))
    def value(vs: Value*): Value =
      vs.foldRight(Unit.value)((hd,tl) => Value.Data(Id, ConstructorId(0), Array(hd,tl)))
  }

  object Optional {
    val Id = org.unisonweb.Id("Optional")
    val noneCid = ConstructorId(0)
    val someCid = ConstructorId(1)
    object None {
      val pattern: Pattern = Pattern.Data(Id, noneCid, Nil)
      val term: Term = Term.Constructor(Id, noneCid)
      val value: Value = Value.Data(Id, noneCid, Array())
    }
    object Some {
      def pattern(p: Pattern): Pattern = Pattern.Data(Id, someCid, List(p))
      def term(t: Term): Term = Term.Constructor(Id, someCid)(t)
      def value(v: Value): Value = Value.Data(Id, someCid, Array(v))
    }
  }

  object Either {
    val Id = org.unisonweb.Id("Either")
    val leftCid = ConstructorId(0)
    val rightCid = ConstructorId(1)
    object Left {
      def pattern(p: Pattern): Pattern = Pattern.Data(Id, leftCid, List(p))
      def term(t: Term): Term = Term.Constructor(Id, leftCid)(t)
      def value(v: Value): Value = Value.Data(Id, leftCid, Array(v))
    }
    object Right {
      def pattern(p: Pattern): Pattern = Pattern.Data(Id, rightCid, List(p))
      def term(t: Term): Term = Term.Constructor(Id, rightCid)(t)
      def value(v: Value): Value = Value.Data(Id, rightCid, Array(v))
    }
  }

  def dataConstructor(id: Id, cid: ConstructorId, arity: Int, paramNames: Name*): ((Id,ConstructorId), Computation) = {
    val body: Computation = arity match {
      case 0 =>
        val data = Value.Data(id, cid, Array.empty)
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          r.boxed = data
          U0
        }
      case 1 =>
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          r.boxed = Value.Data(id, cid, Array(Value.fromParam(x0,x0b)))
          U0
        }
      case 2 =>
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          r.boxed = Value.Data(id, cid, Array(Value.fromParam(x1,x1b), Value.fromParam(x0,x0b)))
          U0
        }
      case n =>
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          // arity = 3, argsStart = top.toInt,     argsStop = top.toInt + 1
          // arity = 4, argsStart = top.toInt - 1, argsStop = top.toInt + 1
          val argsStart = top.toInt - ((arity - compilation.K) + 1)
          val argsStop = top.toInt + 1
          val args = new Array[Value](arity)
          stackU.view(argsStart, argsStop)
                .zip(stackB.view(argsStart,argsStop))
                .map { case (u,b) => Value.fromParam(u,b) }
                .copyToArray(args)
          args(arity - 2) = Value.fromParam(x1,x1b)
          args(arity - 1) = Value.fromParam(x0,x0b)
          r.boxed = Value.Data(id, cid, args)
          U0
        }
    }
    val lam = new Value.Lambda.ClosureForming(arity, body, Term.Constructor(id,cid), Array()) {
      def names = paramNames.toList
    }
    ((id,cid),lam.toComputation)
  }

  val dataConstructorsM: Map[(Id,ConstructorId),Computation] =
    Map(
      dataConstructor(Unit.Id, ConstructorId(0), 0),
      dataConstructor(Tuple.Id, ConstructorId(0), 2, "head", "tail"),
      dataConstructor(Optional.Id, Optional.noneCid, 0),
      dataConstructor(Optional.Id, Optional.someCid, 1, "a"),
      dataConstructor(Either.Id, Either.leftCid, 1, "a"),
      dataConstructor(Either.Id, Either.rightCid, 1, "b")
    )

  val dataConstructors: (Id,ConstructorId) => Computation =
    (id,cid) => dataConstructorsM(id -> cid)
}
