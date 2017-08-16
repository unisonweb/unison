package org.unisonweb

import org.unisonweb.Term.{Name, Term}

package compilation {
  case class Slot(var unboxed: D, var boxed: Value)
  case class Result(var boxed: Value)
}

package object compilation extends TailCalls with CompileLetRec with LookupVar {
  type D = Double
  type V = Value
  type R = Result
  type TC = TailCall

  /**
   * The annotation contains a `Set[Name]` of free variables for the term,
   * and a `Vector[Name]` which is a stack of bound variables at the term
   * (bound variable stack is also called "the environment").
   *
   * Ex: `x -> y -> x + y`, free variables of `x + y` will be `Set(x, y)`,
   * and bound variables will be `Vector(y, x)`.
   */
  type TermC = ABT.AnnotatedTerm[Term.F, (Set[Name], Vector[Name])]

  def unTermC(t: TermC): Term = t.map(_._1)

  def env(t: TermC): Vector[Name] = t.annotation._2

  def freeVars(t: TermC): Set[Name] = t.annotation._1

  def stackSize(t: TermC): Int = stackSize(freeVars(t), env(t))


  /**
   * Given a set of free variables, and a stack of bound variables, figure out
   * how many elements from `bound` stack we need to be able to resolve all free vars.
   *
   * Ex: Set(x,y) and bound = Vector(x,p,q,r,y,z), arity would be: 5, since we need `bound.take(5)`
   * to have access to both `x` and `y`.
   */
  def stackSize(freeVars: Set[Name], bound: Vector[Name]): Int =
    if (freeVars.isEmpty) 0
    else freeVars.view.map(fv => bound.indexOf(fv)).max + 1

//   def compile(builtins: String => Rt)(e: Term): Computation = {
//     compile(ABT.annotateBound(e), BoundByCurrentLambda(), CurrentRec(None), IsTail)
//     // todo: do something with builtins
//   }
//
//   def compile(e: TermC, boundByCurrentLambda: BoundByCurrentLambda, currentRec: CurrentRec, isTail: Boolean): Computation =
//     e match {
//       case Term.Num(n) => compileNum(n)
//       case Term.Builtin(name) => ???
// //      case Term.Compiled(_) => ???
//       case Term.Var(name) =>
//         if (currentRec.contains(name))
//           ??? // compileRec(name)
//         else ??? // compileVar(name, e)
//       case Term.If0(cond, if0, ifNot0) =>
//         ??? //compileIf0(e, boundByCurrentLambda, currentRec)(cond, if0, ifNot0)
//       case Term.Lam(names, body) =>
//         ??? //compileLambda(e, boundByCurrentLambda, currentRec)(names, body)
//       case Term.LetRec(bindings, body) => ???
//       case Term.Let1(name, binding, body) => ???
//       case Term.Apply(fn, args) => ???
//     }

  def compileNum(d: Double) = {
    class CompiledNum extends Computation0(Term.Num(d)) {
      def apply(rec: Lambda, r: R) = d
    }
    new CompiledNum
  }
}
