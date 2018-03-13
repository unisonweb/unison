package org.unisonweb

/*
A tutorial explaining how the Unison runtime works.
We will start with very simple examples of compilers
based on partial evaluation, and work up to the more
complicated representation used in the actual Unison
runtime.

Good background on compilation via partial evaluation
is this Scala World 2017 talk: https://youtu.be/knqlWboqf_U
*/

package object RuntimeTutorial {

  /*
  Will start with a very simple language, which just
  has numbers, addition, and let bindings.

  Later we'll add lambdas and function application,
  let rec, proper tail calls, and more, and make it fast.
  */
  sealed trait Lang0
  type Name = String

  object Lang0 {
    case class Num(n: Double) extends Lang0
    case class Plus(x: Lang0, y: Lang0) extends Lang0
    // let name = e in body
    case class Let1(name: Name, e: Lang0, body: Lang0) extends Lang0
    case class Var(name: Name) extends Lang0
    // note - not doing anything fancy with variable binding

    /*
    A very simple 'compiled form' for `Lang0` is:

      `List[Value] => Value`

    ... where `Value` is just `Double`.
    */

    type Value = Double

    /*
    We call the compiled form of an expression a `Computation`,
    following 'call-by-push-value'.[1]

    Computations are things that must be run (or "normalized")
    to produce a value. Values cannot be further reduced and are
    in _normal form_.
    */
    type Computation = List[Value] => Value

    // [1]: http://requestforlogic.blogspot.com/2011/08/embracing-and-extending-levy-language.html

    def compile(e: Lang0, env: List[Name]): Computation = e match {
      case Num(n) =>
        stack => n
      case Plus(x, y) =>
        val vx = compile(x, env)
        val vy = compile(y, env)
        stack => vx(stack) + vy(stack)
      case Let1(name, b, body) =>
        val cb = compile(b, env)
        val cbody = compile(body, name :: env)
        stack => cbody(cb(stack) :: stack)
      case Var(name) =>
        val i = env.indexOf(name)
        if (i < 0) sys.error(s"Unbound name $name")
        else _(i)
    }

    // let x = 1 + 1 in 12 + x ==> 14
    val ex = compile(Let1("x", Plus(Num(1), Num(1)), Plus(Num(12), Var("x"))), Nil)(List())
  }

  /*
  Things that are inefficient about the above:

    * We're using a purely functional `List[Value]` rather
      than a mutable stack. We're also using a polymorphic
      type for our stack so all the values inside are boxed
      even though they are doubles. These could both be
      fixed, but...
    * Argument-passing in `Let` is using heap-allocated
      `List[Value]`, rather than normal Java call stack.
      Passing arguments via normal Java arg passing is
      going to be much faster - functions can be inlined and
      args passed in registers, the JVM can use special assembly
      instructions for manipulating the call stack...

  We're going to fix these things, but first let's make language
  richer - we'll add lambda, function application, and let rec.
  */

  sealed trait Lang1

  object Lang1 {
    case class Num(n: Double) extends Lang1
    case class Lam(name: Name, body: Lang1) extends Lang1
    case class Apply(fn: Lang1, arg: Lang1) extends Lang1
    // 1 + 2 now represented as Apply(Apply(Plus, 1), 2)
    case object Plus extends Lang1
    case class Let1(name: Name, e: Lang1, body: Lang1) extends Lang1
    case class Var(name: Name) extends Lang1
    // let rec name = e in body -- both `e` and `body` may refer to `name`
    case class LetRec1(name: Name, e: Lang1, body: Lang1) extends Lang1

    type Computation = List[Value] => Value

    /*
    Now our `Value` type is more than just `Double`. Lambdas are values, too!
    */
    trait Value {
      def force: Value = this
      def whnf: Value = this
    }

    object Value {
      case class Num(n: Double) extends Value
      case class Lam(fn: Computation) extends Value
      case class Laz(thunk: () => Value) extends Value {
        override def whnf = thunk()
        override def force = thunk().force
      }
    }

    def compile(e: Lang1, env: List[String]): Computation = e match {
      case Num(n) =>
        val v = Value.Num(n)
        stack => v
      case Plus =>
        lazy val fn: Computation = stack => stack match {
          case Value.Num(y) :: Value.Num(x) :: tl => Value.Num(x + y)
          case Value.Num(x) :: Nil => Value.Lam {
            case Value.Num(y) :: _ => Value.Num(x + y)
            case Nil => fn(Value.Num(x) :: Nil)
            case _ => sys.error("type error")
          }
          case Nil => Value.Lam(fn)
          case _ => sys.error("type error")
        }
        stack => Value.Lam(fn)
      case Apply(Apply(Plus, x), y) =>
        // optimized case - fully saturated call to `+`
        val cx = compile(x, env)
        val cy = compile(y, env)
        stack => cx(stack) match {
          case Value.Num(vx) => cy(stack) match {
            case Value.Num(vy) => Value.Num(vx + vy) // still very slow
            case _ => sys.error("type error")
          }
          case _ => sys.error("type error")
        }
      case Apply(fn, arg) =>
        // The general case - a possibly dynamic call to an unknown
        // function, which arise in higher-order functions.
        // We compute the function, then the arg, push the
        // arg result onto the stack, then call the function.
        val vfn = compile(fn, env)
        val varg = compile(arg, env)
        stack => {
          val Value.Lam(fn) = vfn(stack) // inefficient if `vfn` is statically known
          fn(varg(stack) :: stack)
        }
      case Lam(name, body) =>
        val lam = Value.Lam(compile(body, name :: env))
        _ => lam
      case Let1(name, b, body) =>
        val cb = compile(b, env)
        val cbody = compile(body, name :: env)
        stack => cbody(cb(stack) :: stack)
      case Var(name) =>
        val i = env.indexOf(name)
        if (i < 0) sys.error(s"Unbound name $name")
        else _(i).force
      case LetRec1(name, b, body) =>
        val cb = compile(b, name :: env)
        val cbody = compile(body, name :: env)
        def r(stack: List[Value]) = {
          lazy val bv: Value = cb(Value.Laz(() => bv) :: stack)
          cbody(bv :: stack)
        }
        r(_)
      }
  }

  /*

  Aside from the inefficiencies mentioned previously, it's important
  to optimize for function calls where the function being called is
  statically-known. Dynamic calls to unknown functions are slow - the
  function can't be inlined into the call site (which means fewer
  optimizations) and there's more CPU pipeline stalls since the CPU
  can't predict what instructions will be executed at the dynamic call site.

  Another problem: if we try writing a loop with `let rec`, it will bomb.
  We need a story for supporting tail calls.

  To get tail calls, we just amend our `Computation` type. Currently,
  it's `List[Value] => Value`. We could change that to:

    case class Computation(run: List[Value] => Either[TailCall, Value]) {
      @annotation.tailrec
      final def apply(stack: List[Value]): Value = run(stack) match {
        case Left(more) => more.fn(more.args)
        case Right(v) => v
      }
    }
    case class TailCall(fn: Computation, args: List[Value])

  Running a `Computation` yields either a value or a tail call. Callers
  in non-tail position interpret this in a loop.

  Note: `type Computation = List[Value] => Trampoline[Value]` is also possible.
  This would be very slow - we aren't using the Java stack at all,
  we're heap allocating willy-nilly and EVERY function call is a dynamic call!
  */

  /*

  Here's pretty close to the actual current representation. Notes:

    * We use the Java call stack to pass arguments. Each Unison argument
      is converted to 2 Java function parameters, one for the case where
      that argument is boxed, and one for the case where it is unboxed.
      Only 1 will be set.
        * Though this is the most efficient way to pass arguments (vs
          passing around a single heap-allocated mutable stack), it
          means it gets very tedious / repetitive to write our compiler
          unless we use code generation.
        * Thus, we use code generation to generate the Scala code for our
          compiler.
    * Computations return an unboxed result. If they need to return a boxed
      result, they return that "on the left" by setting a mutable field.
    * To make a tail call requires no allocation. We set mutable fields in
      `Result` then throw a `TailCall` exception
    * We could extend this representation to support multiple kinds of
      unboxed data, but you can get most of the benefit of this idea just
      with a single machine-word sized unboxed type (which can be bitwise
      converted to other machine-word-or-smaller unboxed types). For instance,
      `Long` can be bitwise-converted to `Double` and vice versa.
    * The `rec` argument is the current enclosing lambda. Useful for
      optimizing self-recursive calls.

  type Unboxed = Double // (or Long)

  trait Computation {
    def apply(rec: Lambda, x1: Unboxed, x1b: Value, r: Result): Unboxed
    def apply(rec: Lambda, x1: Unboxed, x1b: Value, x2: Unboxed, x2b: Value, r: Result): Unboxed
    // ...
  }

  case class Result(var boxed: Value, var tailFn: Lambda,
                                      var tailArg1: Unboxed,
                                      var tailArg1b: Value,
                                      var tailArg2: Unboxed,
                                      var tailArg2b: Value,
                                      var rest: Array[Slot])

  case class Slot(unboxed: Unboxed, boxed: Value)
  case object TailCall extends Throwable { override def fillInStackTrace = this }

  */

  // TODO: discuss how decompilation and binary serialization factor in
  // also algebraic effects, ANF conversion, n-ary let rec, external/foreign references
}
