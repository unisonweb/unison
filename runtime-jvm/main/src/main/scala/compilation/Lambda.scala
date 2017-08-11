package org.unisonweb.compilation

import org.unisonweb.Term.{Name, Var}

object Lambda {
  def compileLambda(
    builtins: String => Rt, e: TermC, boundByCurrentLambda0: Option[Set[Name]],
    recursiveVars0: Set[Name], currentRec0: Option[(Name,Arity)])(names: List[Name], body: TermC): Rt = {
    val boundByCurrentLambda = Some(names.toSet)
    val recursiveVars = recursiveVars0 -- names
    val currentRec = shadowsRec(currentRec0, names)
    def makeCompiledBody = compile(builtins, body, boundByCurrentLambda, recursiveVars, currentRec, IsTail)
    lazy val eUnC = unTermC(e)
    lazy val bodyUnC = unTermC(body)
    def makeLambda = names match {
      case name1 :: tl => tl match {
        case Nil => new Lambda1(name1, eUnC, makeCompiledBody)
        case name2 :: tl => tl match {
          case Nil => new Lambda2(name1, name2, eUnC, bodyUnC, makeCompiledBody, builtins)
          case name3 :: tl => tl match {
            case Nil => new Lambda3(name1, name2, name3, eUnC, bodyUnC, makeCompiledBody, builtins)
            case name4 :: tl => tl match {
              case Nil => new Lambda4(name1, name2, name3, name4, eUnC, bodyUnC, makeCompiledBody, builtins)
              case _ => new LambdaN(names.toArray, eUnC, bodyUnC, makeCompiledBody, builtins)
            }
          }
        }
      }
      case Nil => sys.error("impossible")
    }
    if (freeVars(e).isEmpty) makeLambda
    else {
      val locallyBound = freeVars(body).filter(v => !recursiveVars.contains(v))
      (arity(locallyBound, env(e)) : @annotation.switch) match {
        case 0 => makeLambda
        case 1 => new Arity1(e,()) with AccumulateBound {
          val v = locallyBound.toList.head
          val compiledVar = lookupVar(0, v, Var(v))
          def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
            val lam = makeLambda
            lam.bind(bound + (v -> r.toRuntime(compiledVar(rec, x1, x1b, r))))
            r.boxed = lam
            0.0
          }
        }
        case 2 => new Arity2(e,()) with AccumulateBound {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), v, Var(v))) }.toArray
          def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> r.toRuntime(vars(i)._2.apply(rec,x1,x1b,x2,x2b,r)))
              i += 1
            }
            val lam = makeLambda
            lam.bind(bound ++ rts)
            r.boxed = lam
            0.0
          }
        }
        case 3 => new Arity3(e,()) with AccumulateBound {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), v, Var(v))) }.toArray
          def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> r.toRuntime(vars(i)._2.apply(rec,x1,x1b,x2,x2b,x3,x3b,r)))
              i += 1
            }
            val lam = makeLambda
            lam.bind(bound ++ rts)
            r.boxed = lam
            0.0
          }
        }
        case 4 => new Arity4(e,()) with AccumulateBound {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), v, Var(v))) }.toArray
          def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> r.toRuntime(vars(i)._2.apply(rec,x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)))
              i += 1
            }
            val lam = makeLambda
            lam.bind(bound ++ rts)
            r.boxed = lam
            0.0
          }
        }
        case n => new ArityN(n,e,()) with AccumulateBound {
          val vars = locallyBound.view.map { v => (v, lookupVar(env(e).indexOf(v), v, Var(v))) }.toArray
          def apply(rec: Rt, args: Array[Slot], r: R) = {
            var i = 0; var rts = Map[Name,Rt]()
            while (i < vars.length) {
              rts = rts + (vars(i)._1 -> r.toRuntime(vars(i)._2.apply(rec,args, r)))
              i += 1
            }
            val lam = makeLambda
            lam.bind(bound ++ rts)
            r.boxed = lam
            0.0
          }
        }
      }
    }
  }
}