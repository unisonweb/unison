package org.unisonweb

import Term.{Name, Term}
import Term.Syntax._
import compilation.{Computation, Return, Requested}

object BuiltinTypes {

  abstract class Constructor(_cid: Int) {
    val cid = ConstructorId(_cid)
  }

  object Unit extends Constructor(0) {
    val Id = org.unisonweb.Id("()")
    val pattern: Pattern = Pattern.Data(Id, cid, Nil)
    val term: Term = Term.Constructor(Id, cid)
    val value: Value = Value.Data(Id, cid, Array())
  }

  /* Tuple.pattern(Unit.pattern, Optional.Some.pattern(Pattern.Wildcard)) */
  object Tuple extends Constructor(0) {
    val Id = org.unisonweb.Id("Pair")
    def consPattern(hd: Pattern, tl: Pattern): Pattern =
      Pattern.Data(Id, cid, List(hd,tl))
    def pattern(ps: Pattern*): Pattern =
      ps.foldRight(Unit.pattern)((hd,tl) => Pattern.Data(Id, cid, List(hd,tl)))
    def term(ts: Term*): Term =
      ts.foldRight(Unit.term)((hd,tl) => Term.Constructor(Id, cid)(hd,tl))
    def lambda: Term =
      dataConstructors(Id -> cid) match { case Return(lam : Value.Lambda) => Term.Compiled(lam) }
    def value(vs: Value*): Value =
      vs.foldRight(Unit.value)((hd,tl) => Value.Data(Id, cid, Array(hd,tl)))
  }

  object Optional {
    val Id = org.unisonweb.Id("Optional")
    object None extends Constructor(0) {
      val pattern: Pattern = Pattern.Data(Id, cid, Nil)
      val term: Term = Term.Constructor(Id, cid)
      val value: Value = Value.Data(Id, cid, Array())
    }
    object Some extends Constructor(1) {
      def pattern(p: Pattern): Pattern = Pattern.Data(Id, cid, List(p))
      def term(t: Term): Term = Term.Constructor(Id, cid)(t)
      def value(v: Value): Value = Value.Data(Id, cid, Array(v))
    }
  }

  object Either {
    val Id = org.unisonweb.Id("Either")
    object Left extends Constructor(0) {
      def pattern(p: Pattern): Pattern = Pattern.Data(Id, cid, List(p))
      def term(t: Term): Term = Term.Constructor(Id, cid)(t)
      def value(v: Value): Value = Value.Data(Id, cid, Array(v))
    }
    object Right extends Constructor(1) {
      def pattern(p: Pattern): Pattern = Pattern.Data(Id, cid, List(p))
      def term(t: Term): Term = Term.Constructor(Id, cid)(t)
      def value(v: Value): Value = Value.Data(Id, cid, Array(v))
    }
  }

  object Effects {

    object State {
      val Id = org.unisonweb.Id("State")
      object Get extends Constructor(0) {
        def pattern(k: Pattern): Pattern =
          Pattern.EffectBind(Id, cid, List(), k)
        def term: Term = Term.Request(Id, cid)
      }
      object Set extends Constructor(1) {
        def pattern(p: Pattern, k: Pattern): Pattern =
          Pattern.EffectBind(Id, cid, List(p), k)
        def term(t: Term): Term = Term.Request(Id, cid)(t)
      }
    }

    object Read {
      val Id = org.unisonweb.Id("Read")
      object Read extends Constructor(0) {
        def pattern(k: Pattern): Pattern =
          Pattern.EffectBind(Id, cid, List(), k)
        def term: Term = Term.Request(Id, cid)
      }
    }

    object Write {
      val Id = org.unisonweb.Id("Write")
      object Write extends Constructor(0) {
        def pattern(v: Pattern, k: Pattern): Pattern =
          Pattern.EffectBind(Id, cid, List(v), k)
        def term(t: Term): Term = Term.Request(Id, cid)(t)
      }
    }
  }

  def dataConstructorish(req: (Array[Value]) => Value, decompile: Term,
                         paramNames: Name*): Computation = {
    val arity = paramNames.length
    val body: Computation = arity match {
      case 0 =>
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          r.boxed = req(Array())
          U0
        }
      case 1 =>
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          r.boxed = req(Array(Value.fromParam(x0,x0b)))
          U0
        }
      case 2 =>
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          r.boxed =
            req(Array(Value.fromParam(x1,x1b), Value.fromParam(x0,x0b)))
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
          r.boxed = req(args)
          U0
        }
    }
    val lam: Computation =
      if (arity >= 1)
        new Value.Lambda.ClosureForming(paramNames.toList, body, decompile)
          .toComputation
      else try {
        Return(req(Array()))
      } catch { case e@Requested(_,_,_,_) =>
        (r => throw e):Computation.C0
      }
    lam
  }

  /** Creates a new lambda for the given data constructor - the lambda captures
    * arity arguments, then sticks them in a Value.Data with the given Id and
    * ConstructorId.
    *
    * Note: if arity is 0, it just returns the Value.Data directly.
    */
  def dataConstructor(id: Id, cid: ConstructorId,
                      paramNames: Name*): ((Id,ConstructorId), Computation) = {
    val arity = paramNames.length
    val decompile = Term.Constructor(id, cid)
    val x = arity match {
      case 0 =>
        val data = Value.Data(id, cid, Array.empty)
        dataConstructorish(_ => data, decompile, paramNames:_*)
      case n =>
        dataConstructorish(Value.Data(id,cid,_), decompile, paramNames:_*)
    }
    ((id, cid), x)
  }

  /** Creates a new lambda for the given effect request - the lambda captures
    * arity arguments, then sticks them in a Requested which it throws.
    *
    * Note: if arity is 0, it just throws.
    */
  def effectRequest(id: Id, cid: ConstructorId,
                    paramNames: Name*): ((Id,ConstructorId), Computation) = {
    val decompile = Term.Request(id, cid)
    val x = dataConstructorish(
      args => throw(Requested(id, cid, args, Value.Lambda.identity)),
      decompile, paramNames:_*)
    ((id, cid), x)
  }

  val dataConstructors: Map[(Id,ConstructorId),Computation] =
    Map(
      dataConstructor(Unit.Id, Unit.cid),
      dataConstructor(Tuple.Id, Tuple.cid, "head", "tail"),
      dataConstructor(Optional.Id, Optional.None.cid),
      dataConstructor(Optional.Id, Optional.Some.cid, "a"),
      dataConstructor(Either.Id, Either.Left.cid, "a"),
      dataConstructor(Either.Id, Either.Right.cid, "b")
    )

  val effects: Map[(Id,ConstructorId),Computation] = {
    import Effects._
    Map(
      effectRequest(State.Id, State.Get.cid),
      effectRequest(State.Id, State.Set.cid, "state"),
      effectRequest(Read.Id, Read.Read.cid),
      effectRequest(Write.Id, Write.Write.cid, "w")
    )
  }

}
