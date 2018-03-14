package org.unisonweb

import Term.{Term,Name}

object compilation2 {

  type U = Double // unboxed values
  type B = Param // boxed values
  type R = Result

  val K = 4

  abstract class Computation(val decompile: Term) {
    def apply(rec: Lambda, x0: U, x1: U, x2: U, x3: U, stackU: Array[U],
                           x0b: B, x1b: B, x2b: B, x3b: B, stackB: Array[B],
                           r: R): U
  }
  case class Return(v: Value) extends Computation(v.decompile) {
    val c = v match {
      case Num(n) => compile(_ => ???)(Term.Num(n), Vector.empty, false)
      case f : Lambda => new Computation(f.decompile) {
        def apply(rec: Lambda, x0: U, x1: U, x2: U, x3: U, stackU: Array[U],
                               x0b: B, x1b: B, x2b: B, x3b: B, stackB: Array[B],
                               r: R): U = { r.boxed = f; U0 }
      }
    }
    def apply(rec: Lambda, x0: U, x1: U, x2: U, x3: U, stackU: Array[U],
                           x0b: B, x1b: B, x2b: B, x3b: B, stackB: Array[B],
                           r: R): U = c(rec, x0, x1, x2, x3, stackU,
                                             x0b, x1b, x2b, x3b, stackB, r)
  }

  // todo - could pass info here about the type of variable, whether it is boxed or
  // unboxed, and optimize for this case
  def compileVar(e: Term, name: Name, env: Vector[Name]): Computation = env.indexOf(name) match {
    case -1 => sys.error("unbound name: " + name)
    case 0 => new Computation(e) {
      def apply(rec: Lambda, x0: U, x1: U, x2: U, x3: U, stackU: Array[U],
                             x0b: B, x1b: B, x2b: B, x3b: B, stackB: Array[B],
                             r: R): U = { if (x0b ne null) r.boxed = x0b.toValue; x0 }

    }
    case 1 => new Computation(e) {
      def apply(rec: Lambda, x0: U, x1: U, x2: U, x3: U, stackU: Array[U],
                             x0b: B, x1b: B, x2b: B, x3b: B, stackB: Array[B],
                             r: R): U = { if (x1b ne null) r.boxed = x1b.toValue; x1 }

    }
    case 2 => new Computation(e) {
      def apply(rec: Lambda, x0: U, x1: U, x2: U, x3: U, stackU: Array[U],
                             x0b: B, x1b: B, x2b: B, x3b: B, stackB: Array[B],
                             r: R): U = { if (x2b ne null) r.boxed = x2b.toValue; x2 }

    }
    case 3 => new Computation(e) {
      def apply(rec: Lambda, x0: U, x1: U, x2: U, x3: U, stackU: Array[U],
                             x0b: B, x1b: B, x2b: B, x3b: B, stackB: Array[B],
                             r: R): U = { if (x3b ne null) r.boxed = x3b.toValue; x3 }

    }
    case n => val m = n - K; new Computation(e) {
      def apply(rec: Lambda, x0: U, x1: U, x2: U, x3: U, stackU: Array[U],
                             x0b: B, x1b: B, x2b: B, x3b: B, stackB: Array[B],
                             r: R): U = { if (stackB(m) ne null) r.boxed = stackB(m).toValue; stackU(m) }

    }
  }

  abstract class Push1U { def apply(arr: Array[U], u: U): Array[U] }
  abstract class Push1B { def apply(arr: Array[B], u: B): Array[B] }

  def push1U(env: Vector[Name], e: Term): Push1U =
    // if x3 is garbage or we no longer care about it, avoid pushing todo - revisit
    if (env.length < K /*|| !Term.freeVars(e).contains(env(3))*/) new Push1U {
      def apply(arr: Array[U], u: U) = arr
    }
    else new Push1U { val i = env.length - K; def apply(arr: Array[U], u: U) = { arr(i) = u; arr } }

  def push1B(env: Vector[Name], e: Term): Push1B =
    // if x3 is garbage or we no longer care about it, avoid pushing
    if (env.length < K /*|| !Term.freeVars(e).contains(env(3))*/) new Push1B {
      def apply(arr: Array[B], b: B) = arr
    }
    else new Push1B { val i = env.length - K; def apply(arr: Array[B], b: B) = { arr(i) = b; arr } }

  def compile(builtins: Name => Computation)(
      e: Term, env: Vector[Name], isTail: Boolean): Computation =
    e match {
      case Term.Num(n) => new Computation(e) {
        def apply(rec: Lambda, x1: U, x2: U, x3: U, x4: U, stackU: Array[U],
                               x1b: B, x2b: B, x3b: B, x4b: B, stackB: Array[B],
                               r: R): U = n
      }
      case Term.Builtin(name) => builtins(name)
      case ABT.Tm(Term.F.Compiled2_(c)) => Return(c.toValue)
      case Term.Self(name) => new Computation(e) {
        def apply(rec: Lambda, x0: U, x1: U, x2: U, x3: U, stackU: Array[U],
                               x0b: B, x1b: B, x2b: B, x3b: B, stackB: Array[B],
                               r: R): U = { r.boxed = rec; U0 }
      }
      case Term.Var(name) => compileVar(e, name, env)
      case Term.Let1(name, b, body) =>
        val cb = compile(builtins)(b, env, isTail = false)
        val cbody = compile(builtins)(body, name +: env, isTail)
        val pushU = push1U(env, body)
        val pushB = push1B(env, body)
        // todo - compute pushU and pushB during compilation
        new Computation(e) {
          def apply(rec: Lambda, x0: U, x1: U, x2: U, x3: U, stackU: Array[U],
                                 x0b: B, x1b: B, x2b: B, x3b: B, stackB: Array[B],
                                 r: R): U = {
            // todo - handle tail calls here
            val rb = cb(rec, x0, x1, x2, x3, stackU,
                             x0b, x1b, x2b, x3b, stackB, r)
            val rbb = r.boxed
            cbody(rec, rb, x0, x1, x2, pushU(stackU, x3),
                       rbb, x0b, x1b, x2b, pushB(stackB, x3b), r)
          }
        }
      case Term.Lam(names, body) =>
        if (Term.freeVars(e).isEmpty) {
          val cbody = compile(builtins)(body, names.reverse.toVector, isTail = true)
          Return(Lambda(names.length, cbody, e))
        }
        else
          ???
          // todo if e has any free vars, compile and substitute them away
      case Term.Apply(Term.Apply(fn, args), args2) =>
        compile(builtins)(Term.Apply(fn, (args ++ args2):_*), env, isTail)
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
