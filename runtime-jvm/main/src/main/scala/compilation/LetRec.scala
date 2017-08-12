package org.unisonweb.compilation
package x

import org.unisonweb.Term.Name

object CompileLetRec {
  def compileLetRec(builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
    recursiveVars: Set[Name], currentRec: Option[(Name,Arity)], isTail: Boolean)(
    bindings: List[(Name,TermC)], body: TermC): Rt = ??? /*{
    // ex:
    //   let rec
    //     blah = 42
    //     let rec
    //       ping x = pong (x + 1)
    //       pong x = ping (x - 1)
    //       ping blah
    // ping = (let rec ping = ...; pong = ...; ping)
    // todo: am hazy on how we are using recursive vars to decompile
    val recursiveVars2 = recursiveVars ++ bindings.view.map(_._1).map { name =>
      val bindings2: List[(Name, Term)] = bindings map { case (name, b) => (name, b map (_._1)) }
      // easiest to compute annotateBound 'locally', then fixup by adding parent scopes bound vars
      val appendEnv = (p: (Set[Name], Vector[Name])) => (p._1, p._2 ++ env(e)) // parent scopes vars appear later in the stack
      (name, ABT.annotateBound(LetRec(bindings2:_*)(Var(name))) map appendEnv)
    }
    val boundByCurrentLambda2 = boundByCurrentLambda map (_ ++ bindings.map(_._1))
    val compiledBindings = bindings.map(_._2).map(e => compile(builtins, e, boundByCurrentLambda2, recursiveVars2, IsNotTail)).toArray
    val compiledBody = compile(builtins, body, boundByCurrentLambda2, recursiveVars2, isTail)
    val names = bindings.map(_._1).toArray
    // todo: consider doing something fancy to avoid needing to iterate over compiledBindings at runtime
    // compile all the bindings and the body
    // to evaluate, evaluate all the bindings, getting back a `Rt` for each
    // then call bind on each
    val compiledBody2 = compiledBody // NB workaround for https://issues.scala-lang.org/browse/SI-10036
    val compiledBindings2 = compiledBindings
    val names2 = names
    trait B { self : Rt =>
      def bind(env: Map[Name,Rt]) = {
        // remove any bindings shadowed in local let rec
        val env2 = env -- names2
        if (env2.nonEmpty) {
          compiledBindings2.foreach(_.bind(env2))
          compiledBody2.bind(env2)
        }
      }
    }
    // observation - most of the time, bindings will be lambdas, so doesn't really matter whether
    // evaluation of bindings is super fast
    // might want to 'de-rec' useless let recs since regular let code is going to be faster probably
    arity(freeVars(e), env(e)) match {
      case 0 => new Arity0(e,()) with B {
        def apply(r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { eval(b, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(r)
        }
      }
      case 1 => new Arity1(e,()) with B {
        def apply(x1: D, x1b: Rt, r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { eval(b, x1, x1b, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(x1, x1b, r)
        }
      }
      case 2 => new Arity2(e,()) with B {
        def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { eval(b, x1, x1b, x2, x2b, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(rec, x1, x1b, x2, x2b, r)
        }
      }
      case 3 => new Arity3(e,()) with B {
        def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { eval(b, x1, x1b, x2, x2b, x3, x3b, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(rec, x1, x1b, x2, x2b, x3, x3b, r)
        }
      }
      case 4 => new Arity4(e,()) with B {
        def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { eval(b, x1, x1b, x2, x2b, x3, x3b, x4, x4b, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(rec, x1, x1b, x2, x2b, x3, x3b, x4, x4b, r)
        }
      }
      case n => new ArityN(n,e,()) with B {
        def apply(rec: Rt, args: Array[Slot], r: R) = {
          val evaluatedBindings = compiledBindings.map(b => { evalN(b, args, r); r.toRuntime })
          val env = names.zip(evaluatedBindings).toMap
          evaluatedBindings.foreach(b => b.bind(env))
          compiledBody.bind(env) // note - compiledBindings expect evaluated bindings to be bound via `bind`
          compiledBody(rec, args, r)
        }
      }
    }
  }*/
}