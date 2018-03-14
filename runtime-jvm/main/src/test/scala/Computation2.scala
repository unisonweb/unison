package org.unisonweb

import Term.{Term,Name}

object computation2 {

  type U = Double // unboxed values
  type B = Param // boxed values
  type R = Result

  abstract class Computation(val decompile: Term) {
    def apply(rec: Lambda, x1: U, x2: U, x3: U, x4: U, stackU: Array[U],
                           x1b: B, x2b: B, x3b: B, x4b: B, stackB: Array[B],
                           r: R): U
  }
  case class Return(v: Value) extends Computation(v.decompile) {
    val c = v match {
      case Num(n) => compile(_ => ???)(Term.Num(n), Nil, false, 0)
      case f : Lambda => new Computation(f.decompile) {
        def apply(rec: Lambda, x1: U, x2: U, x3: U, x4: U, stackU: Array[U],
                               x1b: B, x2b: B, x3b: B, x4b: B, stackB: Array[B],
                               r: R): U = { r.boxed = f; U0 }
      }
    }
    def apply(rec: Lambda, x1: U, x2: U, x3: U, x4: U, stackU: Array[U],
                           x1b: B, x2b: B, x3b: B, x4b: B, stackB: Array[B],
                           r: R): U = c(rec, x1, x2, x3, x4, stackU,
                                             x1b, x2b, x3b, x4b, stackB, r)
  }

  def compile(builtins: Name => Computation)(
      e: Term, env: List[Name], isTail: Boolean, top: Int): Computation =
    e match {
      case Term.Num(n) => new Computation(e) {
        def apply(rec: Lambda, x1: U, x2: U, x3: U, x4: U, stackU: Array[U],
                               x1b: B, x2b: B, x3b: B, x4b: B, stackB: Array[B],
                               r: R): U = n
      }
      case Term.Builtin(name) => builtins(name)
      case Term.Compiled(c) => ??? // new Computation0(unTermC(termC)) { def apply(rec: Lambda, r: R) = c(r) }
      case Term.Self(name) => new Computation(e) {
        def apply(rec: Lambda, x1: U, x2: U, x3: U, x4: U, stackU: Array[U],
                               x1b: B, x2b: B, x3b: B, x4b: B, stackB: Array[B],
                               r: R): U = { r.boxed = rec; U0 }
      }
      case Term.Var(name) => env.indexOf(name) match {
        case -1 => sys.error("unbound name: " + name)
        case 0 => new Computation(e) {
          def apply(rec: Lambda, x1: U, x2: U, x3: U, x4: U, stackU: Array[U],
                                 x1b: B, x2b: B, x3b: B, x4b: B, stackB: Array[B],
                                 r: R): U = { r.boxed = x1b.toValue; x1 }

        }
      }
      case Term.Let1(name, b, body) =>
        val cb = compile(builtins)(b, env, isTail = false, top)
        val cbody = compile(builtins)(body, name :: env, isTail, top + 1)
        new Computation(e) {
          def apply(rec: Lambda, x1: U, x2: U, x3: U, x4: U, stackU: Array[U],
                                 x1b: B, x2b: B, x3b: B, x4b: B, stackB: Array[B],
                                 r: R): U = {
            // todo - handle tail calls here
            val rb = cb(rec, x1, x2, x3, x4, stackU,
                             x1b, x2b, x3b, x4b, stackB, r)
            val rbb = r.boxed
            cbody(rec, rb, x1, x2, x3, pushU(stackU, top)(x4),
                       rbb, x1b, x2b, x3b, pushB(stackB, top)(x4b), r)
          }
        }
      case Term.Lam(names, body) =>
        if (Term.freeVars(e).isEmpty) {
          val cbody = compile(builtins)(body, names.reverse, isTail = true, top = 0)
          Return(Lambda(names.length, cbody, e))
        }
        else
          ???
          // todo if e has any free vars, compile and substitute them away
      case Term.Apply(Term.Apply(fn, args), args2) =>
        compile(builtins)(Term.Apply(fn, (args ++ args2):_*), env, isTail, top)
      case Term.Apply(fn, args) =>
        // static call, fully saturated
        // static call, underapplied
        // static call, overapplied
        // self call, fully saturated
        // dynamic call
        ???
      case Term.LetRec(bindings, body) =>
        ???
    }

  @inline
  def pushU(s: Array[U], i: Int)(u: U): Array[U] = {
    if (i < 4) s
    else { s(i - 4) = u; s } // todo: perhaps just s(i), so no arithmetic
  }
  @inline
  def pushB(s: Array[B], i: Int)(v: B): Array[B] = {
    if (i < 4) s
    else { s(i - 4) = v; s }
  }

  val U0 = 0.0

  abstract class Param {
    def toValue: Value
  }

  class Ref(val name: Name, computeValue: () => Value) extends Param {
    lazy val value = computeValue()
    def toValue = value
  }
  abstract class Value extends Param {
    def toValue = this
    def decompile: Term
  }
  case class Num(n: U) extends Value {
    def decompile = Term.Num(n)
  }
  case class Lambda(arity: Int, body: Computation, decompile: Term) extends Value

  case object SelfCall extends Throwable { override def fillInStackTrace = this }
  case object TailCall extends Throwable { override def fillInStackTrace = this }

  case class Result(var boxed: Value,
                    var f: Lambda, var x1: U, var x2: U, var xs: Array[U],
                                   var x1b: B, var x2b: B, var xsb: Array[B])
}
