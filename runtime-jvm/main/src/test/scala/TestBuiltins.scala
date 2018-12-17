package org.unisonweb

import org.unisonweb.BuiltinTypes.{Constructor, dataConstructor, effectRequest}
import org.unisonweb.compilation.Computation
import Term.Term
import Term.Syntax._

object TestBuiltins {
  val dataConstructors: Map[(Id,ConstructorId),Computation] =
    Map(
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


}
