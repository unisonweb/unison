package org.unisonweb

import org.unisonweb.Term.{MatchCase, Name, Term}
import org.unisonweb.Value.Lambda

package object compilation {

  type Arity = Int
  type IsTail = Boolean
  val IsTail = true
  val IsNotTail = false

  val K = 2

  case class CurrentRec(get: Option[(Name, Arity)]) extends AnyVal {
    def isEmpty = get.isEmpty
    def contains(name: Name): Boolean = get.exists(_._1 == name)
    def contains(name: Name, arity: Arity): Boolean = get == Some((name, arity))

    /*
     * Knock out the currentRec if appropriate; if it is shadowed,
     * it won't be called
     */
    def shadow(name: Name): CurrentRec = CurrentRec(get.filterNot(name == _._1))

    /*
     * Knock out the currentRec if appropriate; if it is shadowed,
     * it won't be called
     */
    def shadow(names: Seq[Name]): CurrentRec =
      CurrentRec(get.filterNot(names contains _._1))

    /*
     * Create a `RecursiveVars` containing just this `CurrentRec`, if defined,
     * otherwise `RecursiveVars.empty`.
     */
    def toRecursiveVars: RecursiveVars =
      get.map { case (name,_) => RecursiveVars(Set(name)) }
         .getOrElse(RecursiveVars.empty)
  }
  object CurrentRec {
    def none = CurrentRec(None)
    def apply(name: Name, arity: Arity): CurrentRec =
      CurrentRec(Some((name,arity)))
  }

  case class RecursiveVars(get: Set[Name]) extends AnyVal {
    def ++(names: Seq[Name]): RecursiveVars = RecursiveVars(get ++ names)
    def -(name: Name): RecursiveVars = RecursiveVars(get.filterNot(name == _))
  }
  object RecursiveVars {
    def empty = RecursiveVars(Set())
  }

  // type StackPtr = Int
  class StackPtr private(private val top: Int) extends AnyVal {
    def toInt = top
    @inline final def u(stackU: Array[U], envIndex: Int): U =
      stackU(top - envIndex + K)
    @inline final def b(stackB: Array[B], envIndex: Int): B =
      stackB(top - envIndex + K)
    @inline final def incBy(by: Int) = {
      assert(by >= 0)
      new StackPtr(top+by)
    }
    @inline final def inc = incBy(1)
    @inline final def push1(stackU: Array[U], stackB: Array[B], u: U, b: B): Unit = {
      push1U(stackU, u)
      push1B(stackB, b)
    }
    @inline final def pushU(stackU: Array[U], i: Int, u: U): Unit =
      stackU(top + i + 1) = u
    @inline final def push1U(stackU: Array[U], u: U): Unit =
      stackU(top + 1) = u
    @inline final def pushB(stackB: Array[B], i: Int, b: B): Unit =
      stackB(top + i + 1) = b
    @inline final def push1B(stackB: Array[B], b: B): Unit =
      stackB(top + 1) = b
  }
  object StackPtr {
    def empty = new StackPtr(-1)
  }

  case object TailCall extends Throwable { override def fillInStackTrace = this }
  case class Requested(id: Id,
                       constructor: ConstructorId,
                       args: Array[Value],
                       continuation: Lambda) extends Throwable
//  { override def fillInStackTrace = this }

  case class Result(
    var boxed: Value = null,
    var tailCall: Lambda = null,
    var argsStart: StackPtr = StackPtr.empty,
    var stackArgsCount: Int = 0,
    var x1: U = U0, var x0: U = U0,
    var x1b: B = null, var x0b: B = null)

  import Value.Lambda

  /**
    * Computations take a logical stack as an argument. The stack grows
    * to the right, and it is split into an unboxed and boxed representation.
    * The top K elements of the stack are passed as regular function arguments;
    * elements below that are stored in the arrays `stackU` and `stackB`.
    *
    * More specifically:
    *
    *   - `x0` is the top (unboxed) element of the stack
    *   - `x0b` is the top (boxed) element of the stack
    *   - `x1` is immediately below `x0` in the stack
    *   - `stack(top) is immediately below `x1` in the stack
    *   - `stack(top - 1)` is immediately below `stack(top)`, etc.
    *   - To retrieve the ith element (0-based) of the environment is `stack(top - (i - K) - 1)`
    */
  abstract class Computation { self =>
    def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U
  }

  object Computation {
    // Special cases for computations that take unboxed arguments and produce unboxed results,
    // and which are guaranteed not to throw tail call exceptions during evaluation. We check for
    // these in various places to emit more efficient code for common cases.
    abstract class C2U extends Computation {
      def apply(r: R, x1: U, x0: U): U
      final def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U =
        apply(r, x1, x0)
    }
    abstract class C1U extends C2U {
      def apply(r: R, x0: U): U
      final def apply(r: R, x1: U, x0: U): U = apply(r, x0)
    }
    abstract class C0U extends C1U {
      def apply(r: R): U
      final def apply(r: R, x0: U): U = apply(r)
    }
    abstract class C2P extends Computation {
      def apply(r: R, x1: U, x0: U, x1b: B, x0b: B): U
      final def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U =
        apply(r, x1, x0, x1b, x0b)
    }
    abstract class C1P extends Computation {
      def apply(r: R, x0: U, x0b: B): U
      final def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U =
        apply(r, x0, x0b)
    }
    abstract class C0 extends Computation {
      def apply(r: R): U
      final def apply(r: R, rec: Lambda, top: StackPtr,
                      stackU: Array[U], x1: U, x0: U,
                      stackB: Array[B], x1b: B, x0b: B): U =
        apply(r)
    }
  }

  abstract class CompiledCase {
    def apply(scrutinee: U, scrutineeB: Value, r: R, rec: Lambda, top: StackPtr,
              stackU: Array[U], x1: U, x0: U,
              stackB: Array[B], x1b: B, x0b: B): U
  }

  /** Put `v` into `r.boxed` */
  @inline private def returnBoxed(r: R, v: Value): U = { r.boxed = v; U0 }

  // todo: think through whether can elide
  @inline private def returnUnboxed(r: R, unboxed: U, t: UnboxedType): U = {
    r.boxed = t
    unboxed
  }

  @inline private def returnBoth(r: R, x0: U, x0b: B) = {
    if (x0b.isRef)
      x0b.toValue.toResult(r)
    else {
      r.boxed = x0b.toValue
      x0
    }
  }

  case class Self(name: Name) extends Computation {
    def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U =
      returnBoxed(r, rec)
  }

  // todo: maybe opportunities for more expressive matching by breaking this into sub-cases
  abstract class Return(val value: Value) extends Computation.C0 {
    override def toString = s"Return($value)"
  }

  object Return {
    def apply(v: Value): Computation = v match {
      case Value.Unboxed(d, t) => compileUnboxed(d, t)
      case f => new Return(f) {
        def apply(r: R): U = returnBoxed(r, f)
      }
    }
    def unapply(c: Computation): Option[Value] = {
      c match {
        case r: Return => Some(r.value)
        case _ => None
      }
    }
  }

  def compileUnboxed(n: U, t: UnboxedType): Computation =
    new Return(Value.Unboxed(n, t)) {
      def apply(r: R): U = returnUnboxed(r, n, t)
      override def toString = s"UnboxedComputation($n, $t)"
    }

  /**
    * Returns true if the register should be saved to the stack when pushing.
    * `i = 0` is the top register, `i = 1` is below that, etc.
    */
  private def shouldSaveRegister(i: Int, env: Vector[Name], freeVars: Set[Name]): Boolean =
    // if the register has valid data, and the variable is still used, preserve it
    (i < env.length) && freeVars.contains(env(i))


  abstract class Push {
    def push1: P1
    def push2: P2
    abstract class P1 {
      def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u: U, b: B): Unit
    }
    abstract class P2 {
      def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u1: U, b1: B, u0: U, b0: B): Unit
    }

  }

  def push(env: Vector[Name], freeVars: Set[Name]): Push = new Push {
    def push1 =
      if (!shouldSaveRegister(1, env, freeVars)) new P1 {
        def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u: U, b: B): Unit = {
          // top.pushB(stackB, 0, null) // todo: should we null these out?
        }

      }
      else new P1 {
        // todo: does single abstract method conversion box `u`?
        def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u: U, b: B): Unit = {
          top.pushU(stackU, 0, u)
          top.pushB(stackB, 0, b)
        }
      }
    def push2 = {
      val x1garbage: Boolean = !shouldSaveRegister(1, env, freeVars)
      val x0garbage: Boolean = !shouldSaveRegister(0, env, freeVars)

      if (x1garbage && x0garbage) new P2 {
        def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u1: U, b1: B, u0: U, b0: B): Unit = {
          // top.pushB(stackB, 0, null) // todo: should we null these out?
          // top.pushB(stackB, 1, null)
        }
      }
      else if (x0garbage) new P2 {
        def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u1: U, b1: B, u0: U, b0: B): Unit = {
          top.pushU(stackU, 0, u1)
          top.pushB(stackB, 0, b1)
          // top.pushB(stackB, 1, null)
        }
      }
      else if (x1garbage) new P2 {
        def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u1: U, b1: B, u0: U, b0: B): Unit = {
          top.pushU(stackU, 1, u0)
//          top.pushB(stackB, 0, null)
          top.pushB(stackB, 1, b0)
        }
      }
      else new P2 {
        def apply(top: StackPtr, stackU: Array[U], stackB: Array[B], u1: U, b1: B, u0: U, b0: B): Unit = {
          top.pushU(stackU, 0, u1)
          top.pushB(stackB, 0, b1)
          top.pushU(stackU, 1, u0)
          top.pushB(stackB, 1, b0)
        }
      }
    }
  }

  // may throw CaseNoMatch at runtime
  def compileMatchCase(c: Term.MatchCase[Computation]): CompiledCase = {
    val cpattern = compilePattern(c.pattern)
    val caseBody = c.body
    val cbody: Computation = c.guard match {
      case None => caseBody
      case Some(guard) =>
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
           val guardSuccess = guard(r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
           if (guardSuccess == UTrue)
             caseBody(r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
           else throw CaseNoMatch
        }
    }
    // Eval the pattern to set the correct environment for the body
    // Enter the body with the environment from the result of the pattern
    c.pattern.arity match {
      case 0 => (s,sb,r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        cpattern(s,sb,r,stackU,stackB,top)
        cbody(r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
      }
      case 1 => (s,sb,r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        // When cpattern binds arguments,
        //  it pushes r.x1/x0 to the array as needed.
        // ...seeing as how it doesn't have x1/x0 for some reason that
        //    made sense at the time maybe.
        r.x1 = x1
        r.x1b = x1b
        cpattern(s,sb,r,stackU,stackB,top)
        cbody(r,rec,top.inc,stackU,x0,r.x0,stackB,x0b,r.x0b)
      }
      case n => (s,sb,r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        r.x0 = x0
        r.x0b = x0b
        r.x1 = x1
        r.x1b = x1b
        cpattern(s,sb,r,stackU,stackB,top)
        cbody(r,rec,top.incBy(n),stackU,r.x1,r.x0,stackB,r.x1b,r.x0b)
      }
    }
  }

  abstract class CompiledPattern {
    def apply(scrutinee: U, scrutineeB: Value,
              r: R, stackU: Array[U], stackB: Array[B], top: StackPtr): Unit
  }

  // may throw CaseNoMatch at runtime
  def compilePattern(p: Pattern): CompiledPattern = p match {
    case Pattern.LiteralU(u, typ) =>
      (s,_,r,_,_,_) => if (s != u) throw CaseNoMatch
    case Pattern.Wildcard =>
      (s,sb,r,stackU,stackB,top) => {
        top.push1(stackU, stackB, r.x1, r.x1b)
        r.x1 = r.x0
        r.x1b = r.x0b
        r.x0 = s
        r.x0b = sb
      }
    case Pattern.Uncaptured =>
      (_,_,_,_,_,_) => {}

    // case x of (a,b) -> f a b
    case Pattern.Data(_, constructorId, patterns) =>
      val cpatterns: Array[CompiledPattern] =
        patterns.map(compilePattern).toArray
      val offsets: Array[Int] =
        patterns.map(_.arity).scanLeft(0)(_ + _).toArray // map(v => (v - K) max 0).toArray
      (s,sb,r,stackU,stackB,top) => {
        // The scrutinee better be data, or the typer is broken
        val data = sb.asInstanceOf[Value.Data]
        if (data.constructorId == constructorId) {
          val fields = data.fields
          assert(fields.length == cpatterns.length)
          var i = 0; while (i < fields.length && i < cpatterns.length) {
            val pattern = cpatterns(i)
            val field = fields(i)
            val u = field.toResult(r)
            val b = r.boxed
            pattern(u, b, r, stackU, stackB, top.incBy(offsets(i)))
            i += 1
          }
        }
        else throw CaseNoMatch
      }

    // case x of y@(a,b) -> f y b
    case Pattern.As(p) =>
      val cp = compilePattern(p)
      (s,sb,r,stackU,stackB,top) => {
        // same as `Wildcard`
        top.push1(stackU, stackB, r.x1, r.x1b)
        r.x1 = r.x0
        r.x1b = r.x0b
        r.x0 = s
        r.x0b = sb
        // and then whatever `cp` does
        cp(s,sb,r,stackU,stackB,top.inc)
      }

    case Pattern.EffectPure(p) =>
      val cp = compilePattern(p)
      (s,sb,r,stackU,stackB,top) => sb match {
        case Value.EffectPure(u,v) =>
          cp(u, v, r, stackU, stackB, top)
        case _ => throw CaseNoMatch
      }
    case Pattern.EffectBind(id, constructorId, patterns, k) =>
      val cpatterns: Array[CompiledPattern] =
        patterns.map(compilePattern).toArray
      val kpattern: CompiledPattern = compilePattern(k)
      // todo: make a note on how this works
      val offsets: Array[Int] =
        patterns.map(_.arity).scanLeft(0)(_ + _).toArray
      (s,sb,r,stackU,stackB,top) => sb match {
        case Value.EffectBind(`id`,`constructorId`,args,ek) =>
          assert(args.length == cpatterns.length,
                 s"Constructor $id($constructorId) expected ${cpatterns.length} args, got ${args.length}")
          var i = 0; while (i < args.length && i < cpatterns.length) {
            val pattern = cpatterns(i)
            val arg = args(i)
            val u = arg.toResult(r)
            val b = r.boxed
            pattern(u, b, r, stackU, stackB, top.incBy(offsets(i)))
            i += 1
          }
          kpattern(U0, ek, r, stackU, stackB, top.incBy(offsets(i)))
        case _ => throw CaseNoMatch
      }
  }

  def compileStaticFullySaturatedNontailCall(lam: Lambda, compiledArgs: List[Computation]): Computation =
    compiledArgs match {
      case List(arg) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        val argv = eval(arg, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        val argvb = r.boxed
        lam(r, top, stackU, U0, argv, stackB, null, argvb)
      }
      case List(arg1, arg2) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        val argv1 = eval(arg1, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        val argv1b = r.boxed
        val argv2 = eval(arg2, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        val argv2b = r.boxed
        lam(r, top, stackU, argv1, argv2, stackB, argv1b, argv2b)
      }
      case args =>
        assert(args.length > K)
        val argsArray = args.toArray
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) =>
          doFullySaturatedCallK(lam, argsArray, r, rec, top,
                               stackU, x1, x0,
                               stackB, x1b, x0b)
    }


  def compileFullySaturatedSelfTailCall(compiledArgs: List[Computation]): Computation = {
    val stackArgsCount = compiledArgs.length - K
    compiledArgs match {
      case List(arg) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        val rx0v = eval(arg, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        r.x0 = rx0v
        r.x0b = r.boxed
        r.tailCall = rec
        r.stackArgsCount = stackArgsCount
        throw TailCall
      }
      case List(arg1, arg2) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        val arg1v = eval(arg1, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        val arg1vb = r.boxed
        val rx0v = eval(arg2, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        r.x0 = rx0v
        r.x0b = r.boxed
        r.x1 = arg1v
        r.x1b = arg1vb
        r.tailCall = rec
        r.stackArgsCount = stackArgsCount
        throw TailCall
      }
      case args =>
        val argsArray = args.toArray
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          @inline @annotation.tailrec def go(offset: Int): Unit =
            if (offset < argsArray.length - K) {
              val argv = eval(argsArray(offset), r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
              top.pushU(stackU, offset, argv)
              top.pushB(stackB, offset, r.boxed)
              go(offset + 1)
            }
          go(0)

          // eval from argsArray.length-K to argsArray.length-1
          val arg1v = eval(argsArray(argsArray.length-2), r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          val arg1vb = r.boxed
          val arg0v = eval(argsArray(argsArray.length - 1), r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          r.x0 = arg0v
          r.x0b = r.boxed
          r.x1 = arg1v
          r.x1b = arg1vb
          r.argsStart = top.inc
          r.tailCall = rec
          r.stackArgsCount = stackArgsCount
          throw TailCall
        }
    }
  }

  def compileFullySaturatedSelfNontailCall(compiledArgs: List[Computation]): Computation =
    compiledArgs match {
      case List(arg) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        val argv = eval(arg, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        val argvb = r.boxed
        rec(r, top, stackU, U0, argv, stackB, null, argvb)
      }
      case List(arg1, arg2) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        val argv1 = eval(arg1, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        val argv1b = r.boxed
        val argv2 = eval(arg2, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        val argv2b = r.boxed
        rec(r, top, stackU, argv1, argv2, stackB, argv1b, argv2b)
      }
      case args =>
        assert(args.length > K)
        val argsArray = args.toArray
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) =>
          doFullySaturatedCallK(rec, argsArray, r, rec, top,
                                 stackU, x1, x0, stackB, x1b, x0b)
    }

  // todo fixme
  // self references are all good when a lambda is fully applied
  // when underapplied, we construct a new lambda, however the self
  // references really should still point to the OLD lambda (but we think
  // they aren't)
  // proposed soln: have compileUnderappliedCall return a Lambda that,
  // when called, sets the `rec` parameter to the ORIGINAL lambda,
  // rather than the newly returned lambda
  def compileUnderappliedCall(builtins: Environment)(
    lam: Lambda, compiledArgs: Array[Computation]): Computation = {

    val names = lam.names.toArray
    val argCount = compiledArgs.length
    val remNames = names drop argCount
    assert(argCount < lam.arity)
    (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
      @inline @annotation.tailrec def go(i: Int, substs: Map[Name, Term]): Map[Name, Term] = {
        if (i < argCount) {
          val argv = eval(compiledArgs(i),
                          r, rec, top,
                          stackU, x1, x0,
                          stackB, x1b, x0b)
          val param = Term.Compiled(Param(argv, r.boxed), names(i))
          go(i+1, substs.updated(names(i), param))
        }
        else substs
      }
      // Should do no substitution: (\y y y -> y) 0 0
      // because the third y shadows the first two.
      val substs = go(0, Map.empty) -- remNames
      r.boxed = lam.underapply(builtins)(argCount, substs)
      U0
    }
  }

  @inline def doTailCall(
    fn: Lambda,
    arg: Computation,
    r: R, rec: Lambda, top: StackPtr,
    stackU: Array[U], x1: U, x0: U,
    stackB: Array[B], x1b: B, x0b: B): Nothing = {

    val rx0v = eval(arg, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
    r.x0 = rx0v
    r.x0b = r.boxed
    r.stackArgsCount = 0
    r.tailCall = fn
    throw TailCall
  }

  @inline def doTailCall(
    fn: Lambda,
    arg1: Computation,
    arg2: Computation,
    r: R, rec: Lambda, top: StackPtr,
    stackU: Array[U], x1: U, x0: U,
    stackB: Array[B], x1b: B, x0b: B): Nothing = {

    val arg1v = eval(arg1, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
    val arg1vb = r.boxed
    val rx0v = eval(arg2, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
    r.x0 = rx0v
    r.x0b = r.boxed
    r.x1 = arg1v
    r.x1b = arg1vb
    r.stackArgsCount = 0
    r.tailCall = fn
    throw TailCall
  }

  @inline def doTailCall(
    fn: Lambda,
    args: Array[Computation],
    r: R, rec: Lambda, top: StackPtr,
    stackU: Array[U], x1: U, x0: U,
    stackB: Array[B], x1b: B, x0b: B): Nothing = {

    assert(K == 2) // rewrite all the cases if K is different.

    @inline @annotation.tailrec def go(offset: Int): Unit =
      if (offset < args.length - K) {
        val v = eval(args(offset), r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        top.pushU(stackU, offset, v)
        top.pushB(stackB, offset, r.boxed)
        go(offset + 1)
      }
    go(0)

    val arg1v = eval(args(args.length-2), r, rec, top,
                stackU, x1, x0, stackB, x1b, x0b)
    val arg1vb = r.boxed
    val rx0v = eval(args(args.length - 1), r, rec, top,
                 stackU, x1, x0, stackB, x1b, x0b)
    r.x0 = rx0v
    r.x0b = r.boxed
    r.x1 = arg1v
    r.x1b = arg1vb
    r.argsStart = top.inc
    r.stackArgsCount = args.length - K
    r.tailCall = fn
    throw TailCall
  }

  @inline def doFullySaturatedCall(fn: Lambda,
                                   args: Array[Computation],
                                   r: R, rec: Lambda, top: StackPtr,
                                   stackU: Array[U], x1: U, x0: U,
                                   stackB: Array[B], x1b: B, x0b: B): U = {
    args match {
      case Array(arg) => doFullySaturatedCall1(
        fn, arg, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
      case Array(arg1, arg2) => doFullySaturatedCall2(
        fn, arg1, arg2, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
      case args => doFullySaturatedCallK(
        fn, args, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
    }
  }

  @inline def doFullySaturatedCall1(fn: Lambda,
                                    arg: Computation,
                                    r: R, rec: Lambda, top: StackPtr,
                                    stackU: Array[U], x1: U, x0: U,
                                    stackB: Array[B], x1b: B, x0b: B): U = {

    val argv = eval(arg, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
    val argvb = r.boxed

    fn(r, top, stackU, U0, argv, stackB, null, argvb)
  }

  @inline def doFullySaturatedCall2(fn: Lambda,
                                    arg1: Computation,
                                    arg2: Computation,
                                    r: R, rec: Lambda, top: StackPtr,
                                    stackU: Array[U], x1: U, x0: U,
                                    stackB: Array[B], x1b: B, x0b: B): U = {

    val argv1 = eval(arg1, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
    val argv1b = r.boxed
    val argv2 = eval(arg2, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
    val argv2b = r.boxed

    fn(r, top, stackU, argv1, argv2, stackB, argv1b, argv2b)
  }

  @inline def doFullySaturatedCallK(
    fn: Lambda,
    args: Array[Computation],
    r: R, rec: Lambda, top: StackPtr,
    stackU: Array[U], x1: U, x0: U,
    stackB: Array[B], x1b: B, x0b: B): U = {

    assert(args.length > 2)
    assert(K == 2) // rewrite all the cases if K is different.

    @inline @annotation.tailrec def go(offset: Int): Unit =
      if (offset < args.length - K) {
        val v = eval(args(offset), r, rec, top,
                     stackU, x1, x0, stackB, x1b, x0b)
        top.pushU(stackU, offset, v)
        top.pushB(stackB, offset, r.boxed)
        go(offset + 1)
      }
    go(0)

    val argv1 = eval(args(args.length - K), r, rec, top, stackU,
                 x1, x0, stackB, x1b, x0b)
    val argv1b = r.boxed

    val argv2 = eval(args(args.length - (K-1)), r, rec, top, stackU,
                     x1, x0, stackB, x1b, x0b)
    val argv2b = r.boxed
    fn(r, top.incBy(args.length - K), stackU, argv1, argv2, stackB, argv1b, argv2b)
  }

  // in `f x`
  // - evaluate `f`
  // - notice `f` has arity 1, and that the call is in tail position
  //    - issue tail call to `f`
  // - if `f` has arity > 1
  def dynamicCall(builtins: Environment)(
                  fn: Computation, args: List[Computation], isTail: Boolean): Computation = {
    val argsArray = args.toArray
    (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
      @annotation.tailrec
      def invokeDynamic(fn: Lambda, args: Array[Computation]): U = {
        if (args.length == fn.arity) {
          if (isTail) args.length match {
            case 1 => doTailCall(fn, args(0), r, rec, top,
                                 stackU, x1, x0, stackB, x1b, x0b)
            case 2 => doTailCall(fn, args(0), args(1), r, rec, top,
                                 stackU, x1, x0, stackB, x1b, x0b)
            case _ => doTailCall(fn, args, r, rec, top,
                                 stackU, x1, x0, stackB, x1b, x0b)
          }
          else doFullySaturatedCall(
            fn, args, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        }
        else if (args.length < fn.arity) {
          val c = compileUnderappliedCall(builtins)(fn, args)
          c(r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        }
        else {
          val lam = {
            doFullySaturatedCall(
              fn, args.take(fn.arity), r, rec, top,
              stackU, x1, x0, stackB, x1b, x0b)
            r.boxed.asInstanceOf[Lambda]
          }
          assert(lam != null)
          invokeDynamic(lam, args.drop(fn.arity))
        }
      }

      val evalFn = {
        eval(fn, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        r.boxed
      }.asInstanceOf[Lambda]

      invokeDynamic(evalFn, argsArray)
    }
  }

  def compileLambda(builtins: Environment)
                   (e: Term, env: Vector[Name],
                    currentRec: CurrentRec,
                    bodyRec: CurrentRec,
                    recVars: RecursiveVars,
                    names: List[Name], body: Term): Computation = {

    val compiledLambda =
      compileLambdaImpl(builtins)(e, env, currentRec, bodyRec, recVars, names, body)

    if (hasTailRecursiveCall(currentRec.shadow(names), body)) {
      // let rec
      //   go n = if n == 0 then n else go (n - 1)
      //   go
      // gets converted to:
      // let rec
      //   go-inner n = if n == 0 then n else throw SelfCall (n - 1)
      //   go n = while (true) return { try go-inner n catch { case SelfCall n2 => go-inner n2 }}
      //   go

      def printStack[A](stackU: Array[A], top: StackPtr, x1: A, x0: A, prefix: String = "") = {
        println {
          prefix +
            (0 to top.toInt map stackU).mkString("[",", ", "]") + " " +
            s"($x1) ($x0)"
        }
      }

      def stackRegion[A](stackU: Array[A], start: Int, length: Int, prefix: String = "") =
        prefix + (start until (start+length) map stackU).mkString("[",", ", "]")

      def handleSelfCalls(innerLambda: Lambda): Lambda = {
        assert(names.length == innerLambda.arity)
        val needsCopy = names.length > K
        // inner lambda may throw SelfCall, so we create wrapper Lambda
        // to process those
        val outerLambdaBody: Computation = (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          val stackArgsCount = (innerLambda.arity - K) max 0

          @inline @annotation.tailrec
          def go(x1: U, x0: U, x1b: B, x0b: B): U = {
            try {
              val result = innerLambda(r, top, stackU, x1, x0, stackB, x1b, x0b)
              // todo: could be lazier or more targeted about nulling out the stack
              java.util.Arrays.fill(
                stackB.asInstanceOf[Array[AnyRef]],
                top.toInt + 1,
                stackB.length, null
              )
              return result
            }
            catch {
              case TailCall if r.tailCall eq innerLambda =>
                if (needsCopy) {
                  System.arraycopy(
                    stackU, r.argsStart.toInt,
                    stackU, top.toInt + 1 - stackArgsCount,
                    stackArgsCount
                  )
                  System.arraycopy(
                    stackB, r.argsStart.toInt,
                    stackB, top.toInt + 1 - stackArgsCount,
                    stackArgsCount
                  )
                }
            }
            go(r.x1, r.x0, r.x1b, r.x0b)
          }

          go(x1, x0, x1b, x0b)
        }
        Lambda(names.length, outerLambdaBody, innerLambda.unboxedType, innerLambda.decompile)
      }

      compiledLambda match {
        case Return(innerLambda: Lambda) =>
          Return(handleSelfCalls(innerLambda))

        case compiledLambda => // first evaluate innerLambda within a Computation
          (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
            r.boxed = handleSelfCalls {
              compiledLambda(r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
              r.boxed.asInstanceOf[Lambda]
            }
            U0
          }
      }
    }
    else compiledLambda
  }

  def compileLambdaImpl(builtins: Environment)
                       (e: Term, env: Vector[Name],
                        currentRec: CurrentRec,
                        bodyRec: CurrentRec, // the CurrentRec when compiling body
                        recVars: RecursiveVars,
                        names: List[Name], body: Term): Computation = {
    // There are two cases:
    // 1. The lambda has no free vars (is closed) or has just 1 free var
    //    which is equal to `bodyRec`. In this case, we compile the body
    //    with `bodyRec` as the `CurrentRec`, and the `Return` the resulting
    //    Lambda. `currentRec` is unused in this case.
    //
    //    Ex: `(x y -> x)` is closed, and is case 1
    //    Ex:
    //      let rec fib = n -> if (n < 2) n
    //                         else fib (n - 1) + fib (n - 2)
    //           in fib
    //      Here, the lambda `n -> ...` has 1 free var, `fib`,
    //      which will be equal to `bodyRec` (set by `LetRec` compilation).
    //
    // 2. The lambda has other free vars, which need to be substituted away.
    //    We lookup all these free vars in the current environment, which
    //    has access to `currentRec`. Then we substitute the results into the
    //    body of the lambda. Then compile the lambda, reducing to case 1 above.
    //
    //    Ex: (x -> fib 24), references `fib` declared in some outer scope.
    //    We return a computation that will resolve the reference at runtime,
    //    substitute the result into the lambda, then compile that lambda.
    //    (in other words, we do not form a closure, we specialize the lambda
    //     for whatever its free vars are instantiated to at runtime)
    //

    val freeVars = Term.freeVars(e)
    // 1.
    if (freeVars.isEmpty ||
        freeVars.size == 1 && currentRec.contains(freeVars.head)) {
      val shadowedRec = bodyRec.shadow(names)
      val cbody = compile(builtins)(body, names.reverse.toVector,
        shadowedRec, shadowedRec.toRecursiveVars, IsTail)
      Return(Lambda(names.length, cbody, None, e))
    }
    // 2.
    else {
      val compiledFrees: Map[Name, Computation] =
        (freeVars -- recVars.get).view.map {
          name => (name, compileVar(Term.Var(name), name, env, currentRec))
        }.toMap
      val compiledFreeRecs: Map[Name, ParamLookup] =
        freeVars.intersect(recVars.get).view.map {
          name => (name, compileRef(Term.Var(name), name, env, currentRec))
        }.toMap
      (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        val evaledFreeVars: Map[Name, Term] = compiledFrees.transform {
          (name, c) =>
            val evaluatedVar = c(r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
            val value = Value(evaluatedVar, r.boxed)
            Term.Compiled(value, name)
        }

        val evaledRecVars: Map[Name, Term] = compiledFreeRecs.transform {
          (name, lookup) =>
            val evaluatedVar = lookup(rec, top, stackB, x1b, x0b)
            if (evaluatedVar eq null) sys.error(name + " refers to null stack slot.")
            Term.Compiled(evaluatedVar, name)
        }

        val body2 = ABT.substs(evaledFreeVars ++ evaledRecVars)(body)
        val lam2 = Term.Lam(names: _*)(
          body = body2
        )
        assert(Term.freeVars(lam2).size <= 1)
        r.boxed = compileLambda(builtins)(
          lam2, Vector(), currentRec, bodyRec, recVars, names, body2
        ) match {
          case v: Return => v.value
          case v => sys.error(
            s"""compiling a lambda with no free vars should always produce a Return;
               |instead got $v
             """.stripMargin
          )
        }
        U0
      }
    }
  }

  def normalize(builtins: Environment)(e: Term): Term = {
    val c = compileTop(builtins)(e)
    val v = run(c)
    val x = Term.etaNormalForm(v.decompile)
    Term.fullyDecompile2(x)
  }

  def run(c: Computation): Value = {
    val r = Result()
    val us = new Array[U](1024)
    val bs = new Array[B](1024)
    val cc = eval(c, r, null, StackPtr.empty, us, U0, U0, bs, null, null)
    Value(cc, r.boxed)
  }

  /** Compile top-level term */
  def compileTop(builtins: Environment)(e: Term) =
    compile(builtins)(e, Vector(), CurrentRec.none, RecursiveVars.empty, IsTail)

  case class Environment(
    builtins: Name => Computation,
    userDefined: Hash => Computation,
    dataConstructors: (Id,ConstructorId) => Computation,
    effects: (Id,ConstructorId) => Computation
  )

  def compile(builtins: Environment)(
    e: Term,
    env: Vector[Name] = Vector.empty,
    currentRec: CurrentRec = CurrentRec.none,
    recVars: RecursiveVars = RecursiveVars.empty,
    isTail: IsTail): Computation = {

    e match {
      case Term.Unboxed(n,t) => compileUnboxed(n,t)
      case Term.Text(txt) => Return(Builtins.External(txt, e))
      case Term.Id(Id.Builtin(name)) => builtins.builtins(name)
      case Term.Id(Id.HashRef(h)) => ???
      case Term.Constructor(id,cid) => builtins.dataConstructors(id,cid)
      case Term.Compiled(param,_) =>
        if (param.toValue eq null) // todo: make this C0?
          (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => param.toValue.toResult(r)
        else Return(param.toValue)
      case Term.Sequence(s) =>
        val cs = s.map(compile(builtins)(_, env, currentRec, recVars, IsNotTail))
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          val cv: util.Sequence[Value] = cs.map { c =>
            val cv = c(r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
            val cvb = r.boxed
            Value(cv, cvb)
          }
          r.boxed = Builtins.External(cv)
          U0
        }
      case Term.Var(name) => compileVar(e, name, env, currentRec)
      case Term.If(cond, t, f) =>
        val ccond = compile(builtins)(cond, env, currentRec, recVars, IsNotTail)
        val ct = compile(builtins)(t, env, currentRec, recVars, isTail)
        val cf = compile(builtins)(f, env, currentRec, recVars, isTail)
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) =>
          if (unboxedToBool(eval(ccond, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)))
            ct(r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          else
            cf(r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
      case Term.Match(_, Nil) =>
        sys.error("parser shouldn't produce a match with no cases")
      case Term.Match(scrutinee, cases) =>
        val cscrutinee = compile(builtins)(
          scrutinee, env, currentRec, recVars, IsNotTail
        )
        // case (foo + 1) of 42 -> ... ; 43 -> ...
        val ccases: List[CompiledCase] = cases.map { c =>
          compileMatchCase {
            c.map {
              case ABT.AbsChain(names, t) =>
                val env2 = names.reverse.toVector ++ env
                compile(builtins)(t, env2, currentRec, recVars, isTail)
              case t =>
                compile(builtins)(t, env, currentRec, recVars, isTail)
            }
          }
        }

        def sequenceCases(c1: CompiledCase, c2: CompiledCase): CompiledCase =
          (s,sb,r,rec,top,stackU,x1,x0,stackB,x1b,x0b) =>
            try c1(s, sb, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
            catch { case CaseNoMatch =>
              c2(s, sb, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
            }

        val megamatch: CompiledCase = ccases.reduceRight(sequenceCases)

        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          val vscrutinee = cscrutinee(r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
          val vscrutineeb = r.boxed
          try megamatch(vscrutinee,vscrutineeb,
                        r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
          catch {
            case CaseNoMatch =>
              throw MatchFail(scrutinee, Value(vscrutinee, vscrutineeb).decompile, cases)
          }
        }

      case Term.Let1(name, b, body) =>
        val cb = compile(builtins)(b, env, currentRec, recVars, IsNotTail)
        // cc will have access to current lambda when it it is substituting
        // away any free vars, but the body of `cc` will not have access to the
        // current lambda
        val cc = compile(builtins)(
          Term.Lam(name)(body), env, currentRec, recVars, isTail)
        val cbody = compile(builtins)(body, name +: env, currentRec.shadow(name),
          recVars - name, isTail)
        val push = compilation.push(env, Term.freeVars(body)).push1
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          val rb = try
              // eval the binding
              eval(cb, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
            catch { case Requested(effect, ctor, args, k) =>
              // eval the current continuation
              eval(cc, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
              // Rethrow with composite continuation
              throw Requested(
                effect, ctor, args,
                r.boxed.asInstanceOf[Lambda].compose(k))
            }
          val rbb = r.boxed
          push(top, stackU, stackB, x1, x1b)
          // Call the body
          cbody(r, rec, top.inc, stackU, x0, rb, stackB, x0b, rbb)
        }
      // todo: Let2, etc.

      case lam@Term.Lam(names, body) =>
        compileLambda(builtins)(lam, env,
                               currentRec,
                               // we don't allow body of lambda to use self refs
                               bodyRec = CurrentRec.none,
                               recVars, names, body)

      case Term.Apply(Term.Apply(fn, args), args2) => // converts nested applies to a single apply
        compile(builtins)(Term.Apply(fn, (args ++ args2): _*), env, currentRec, recVars, isTail)

      case Term.Apply(fn, Nil) => sys.error("the parser isn't supposed to produce this")

      case Term.Apply(fn, args) =>
        val compiledArgs: List[Computation] =
          args.view.map(arg => compile(builtins)(arg, env, currentRec, recVars, IsNotTail)).toList

        val cfn: Computation = compile(builtins)(fn, env, currentRec, recVars, IsNotTail)

        cfn match {

          case Return(lam@Lambda(arity, _, _)) =>
            if (args.length == arity)
              // static tail call, fully saturated
              //   (x -> x+4) 42
              //   ^^^^^^^^^^^^^

              // static fully staturated tail calls deemed too slow/unnecessary
              // if (isTail && needsTailCall(fn))
              //   compileStaticFullySaturatedTailCall(lam, compiledArgs)
              // else /* see below */

              // static non-tail call, fully saturated
              //   ex: let x = (x -> x + 1) 1; x
              //               ^^^^^^^^^^^^^^
              lam.saturatedNonTailCall(compiledArgs)

            // static call, underapplied (no tail call variant - just returning immediately)
            //   ex: (x y -> x) 42
            //       ^^^^^^^^^^^^^
            else if (args.length < arity)
              compileUnderappliedCall(builtins)(lam, compiledArgs.toArray)

            // static call, overapplied
            //   ex: (x -> x) (x -> x) (x -> x) 42
            //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            else {
              val fn2: Computation =
                compileStaticFullySaturatedNontailCall(
                  lam, compiledArgs.take(lam.arity))

              dynamicCall(builtins)(fn2, compiledArgs.drop(lam.arity), isTail)
            }

          case _ => fn match {
            case Term.Var(name) =>
              if (currentRec.contains(name, args.length)) {
                if (isTail)
                  compileFullySaturatedSelfTailCall(compiledArgs)
                else
                  // self non-tail call, fully saturated
                  //   ex: let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
                  //                                            ^^^^^^^^^^^   ^^^^^^^^^^^
                  compileFullySaturatedSelfNontailCall(compiledArgs)
              }
              else // catches underapplied self calls (either in tail or non-tail position)
                dynamicCall(builtins)(cfn, compiledArgs, isTail)
            // dynamic call
            //   ex: let apply f x = f x; ...
            //                       ^^^^
            case _ =>
              dynamicCall(builtins)(cfn, compiledArgs, isTail)
          }
        }

      case Term.LetRec(bindings, body) =>
        val bindingNames = bindings.map(_._1).toArray
        val env2 = bindingNames.foldLeft(env) { (env, b) => b +: env }
        val recVars2 = recVars ++ bindingNames
        val currentRec2 = currentRec.shadow(bindingNames)
        val cbindings = bindings.map {
          case (name, lam @ Term.Lam(names, body)) =>
            compileLambda(builtins)(lam, env2,
              currentRec,
              // we do allow body of lambda to refer to `name`
              bodyRec = CurrentRec(name, arity = names.length),
              recVars2, names, body)
          case (_, term) =>
            compile(builtins)(term, env2, currentRec2, recVars2, IsNotTail)
        }.toArray
        val cbody = compile(builtins)(
          body, env2, currentRec2, recVars2, isTail)

        // let x1 = [ a huge boxed value ]
        //     x0 = 2834
        //     let rec
        //       fib n = ...
        //       fib
        cbindings match {
          case Array(cbinding) =>
            val name = bindingNames(0)
            val push = compilation.push(env, Term.freeVars(e)).push1
            (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
              var bindingResult = new Ref(name, null)
              push(top, stackU, stackB, x1, x1b)
              val v = eval(cbinding, r, rec, top.inc,
                           stackU, x0, U0,
                           stackB, x0b, bindingResult)
              bindingResult.value = Value(v, r.boxed)
              cbody(r, rec, top.inc, stackU, x0, U0, stackB, x0b, bindingResult)
            }
          case Array(cbinding1, cbinding0) =>
            val name1 = bindingNames(0)
            val name0 = bindingNames(1)
            val push = compilation.push(env, Term.freeVars(e)).push2
            (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
              val r1 = new Ref(name1, null)
              val r0 = new Ref(name0, null)
              push(top, stackU, stackB, x1, x1b, x0, x0b)
              val top2 = top.incBy(2)
              val r1v = eval(cbinding1, r, rec, top2,
                             stackU, U0, U0, stackB, r1, r0)
              val r1vb = r.boxed
              r1.value = Value(r1v, r1vb)

              val r0v = eval(cbinding0, r, rec, top2,
                             stackU, r1v, x0 = U0, stackB, r1vb, x0b = r0)
              val r0vb = r.boxed
              r0.value = Value(r0v, r0vb)
              cbody(r, rec, top2, stackU, r1v, r0v, stackB, r1vb, r0vb)
            }
          case cbindings =>
            val push = compilation.push(env, Term.freeVars(e)).push2
            assert(K == 2)
            (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
              // 1. Create a Ref to hold the result of each binding
              // 2. Spill K registers onto the array portion of stack
              // 3. Push N-K Refs onto the stack
              // 4. To evaluate a binding (or the body)
              //    a. Put last K Refs in registers, evaluate
              //    b. Set the Ref to the result of evaluating the binding
              // 5. Evaluate body in the same way

              // 1. Create a Ref to hold the result of each binding
              val bindingResults = bindingNames.map(name => new Ref(name, null))

              // 2. Spill K registers onto the array portion of stack
              push(top, stackU, stackB, x1, x1b, x0, x0b)

              // 3. Push N-K Refs onto the stack
              val top2 = top.incBy(K)
              @inline @annotation.tailrec
              def pushRefs(i: Int): Unit = {
                if (i < cbindings.length - K) {
                  top2.pushU(stackU, i, U0)
                  top2.pushB(stackB, i, bindingResults(i))
                  pushRefs(i+1)
                }
              }
              pushRefs(0)

              val topN = top.incBy(cbindings.length)
              val brx1 = bindingResults(bindingResults.length - 2)
              val brx0 = bindingResults(bindingResults.length - 1)
              // 4. To evaluate a binding (or the body)
              //    a. Put last K Refs in registers, evaluate
              //    b. Set the Ref to the result of evaluating the binding
              @inline @annotation.tailrec
              def evalAllButLastKBindings(i: Int): Unit = {
                if (i < cbindings.length - K) {
                  val v = eval(cbindings(i), r, rec, topN,
                               stackU, U0, U0, stackB, brx1, brx0)
                  val vb = r.boxed
                  bindingResults(i).value = Value(v, vb)
                  // mutate the stack to dereference what is currently a Ref
                  // in that position - this ensures that later bindings that
                  // reference this variable get a proper value rather than
                  // a Ref
                  top2.pushU(stackU, i, v)
                  top2.pushB(stackB, i, vb)
                  evalAllButLastKBindings(i+1)
                }
              }
              evalAllButLastKBindings(0)

              // eval last K bindings
              val b1v = eval(cbindings(cbindings.length-2), r, rec, topN,
                             stackU, U0, U0,
                             stackB, brx1, brx0)
              val b1vb = r.boxed
              brx1.value = Value(b1v, b1vb)
              // each successive binding can use one more v/vb pair instead of a ref
              val b0v = eval(cbindings(cbindings.length-1), r, rec, topN,
                             stackU, b1v /* <- */, U0,
                             stackB, b1vb/* <- */, brx0)
              val b0vb = r.boxed
              brx0.value = Value(b0v, b0vb)

              // 5. Evaluate body in the same way
              cbody(r, rec, topN, stackU, b1v, b0v, stackB, b1vb, b0vb)
            }
        }
      case Term.Request(id,cid) => builtins.effects(id,cid)
      case Term.Handle(handler, block) =>
        import Term.Syntax._
        val cHandler =
          compile(builtins)(handler, env, currentRec, recVars, IsNotTail)
        val cBlock =
          compile(builtins)(block, env, currentRec, recVars, isTail)
        new Computation { self =>
          // Installs `handler` around the body of `k`, so given `k`, we
          // produce `k'`:
          //
          //   k = x* -> <blah>
          //   k' = x* -> handle h (k x*)
          //
          // Here, `x*` represents any number of arguments.
          def attachHandler(handler: Lambda, k: Lambda): Lambda = {
            def decompiled =
              // Note: We have to make up a name here. "handler" works.
              Term.Lam(k.names:_*)(Term.Handle(Term.Compiled(handler, "handler"))(
                k.decompile(k.names.map(Term.Var(_)):_*)))
            val body: Computation = (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) =>
              doIt(handler, k.body)(r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
            Lambda(k.arity, body, k.unboxedType, decompiled)
          }
          def apply(r: R, rec: Lambda, top: StackPtr,
                    stackU: Array[U], x1: U, x0: U,
                    stackB: Array[B], x1b: B, x0b: B): U = {
            eval(cHandler,r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
            doIt(r.boxed.asInstanceOf[Lambda], cBlock)(
                 r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          }
          /*
           * Evaluates the `body`. If the body raises `Requested`,
           * passes `EffectBind` of that request to the handler.
           * If `body` completes, pass `EffectPure` to the handler.
           * If the handler can't process the request, it will throw
           * `Requested`, which we catch and rethrow with `handler`
           * installed in the body of the continuation.
           */
          def doIt(handler: Lambda, body: Computation)(
                   r: R, rec: Lambda, top: StackPtr,
                   stackU: Array[U], x1: U, x0: U,
                   stackB: Array[B], x1b: B, x0b: B): U = {
            assert(handler.arity == 1)
            var blockU: U = U0
            var blockB: Value = null
            var bodyCompleted = false
            try {
              blockU = body(r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
              blockB = r.boxed
              bodyCompleted = true
            }
            catch {
              // If the body throws a tail call, install the handler in the
              // tail call.
              case TailCall =>
                r.tailCall = attachHandler(handler, r.tailCall)
                throw TailCall
              // If the body issues a request, we obviously pass it to the
              // handler.
              case Requested(id, ctor, args, k) =>
                val data = Value.EffectBind(id, ctor, args, k)
                try { blockU = handler(r,top,stackU,U0,U0,stackB,null,data) }
                catch {
                  // In case of nested handlers, we need to attach the inner
                  // handler to the continuation in case the inner handler
                  // requests an effect of the outer handler. This is because
                  // the outer handler needs to be able to delegate back to
                  // the inner one when the continuation contains more requests
                  // that need to be caught by the inner handler.
                  //
                  // E.g.
                  //
                  // handle (doSomeIO)
                  //   handle (doSomeState)
                  //     foo
                  //
                  // Here, doSomeState may have IO effects, followed by state
                  // effects which doSomeIO doesn't know how to handle.
                  case Requested(id, ctor, args, k) =>
                    // we augment `k` to be wrapped in `h` handler
                    // k' = x -> handle h (k x)
                    throw Requested(id, ctor, args, attachHandler(handler, k))
                  // The handler didn't handle this request. Maybe it will be
                  // handled by an outer handler. Rethrow, but attach this
                  // handler to the continuation.
                  case m@MatchFail(_,_,_) =>
                    throw Requested(id, ctor, args, attachHandler(handler, k))
                }
            }
            if (bodyCompleted) try {
              handler(r,top,stackU,U0,U0,stackB,null,
                      Value.EffectPure(blockU, blockB))
            } catch {
              case MatchFail(_,_,_) =>
                sys.error(
                  """Handler didn't handle a pure effect.
                   | The compiler should statically prevent this.""".stripMargin)
            } else blockU
          }
        }
    }
  }

  /** Returns `true` if a fully saturated call of `fn` should be compiled as a tail call. */
  def needsTailCall(fn: Term): Boolean = fn match {
    // recursive calls must always go through a `Var` or `Self`, and recursive calls are the
    // only calls required to be emitted as tail calls to ensure stack safety
    // (you can create cycles via higher order functions, but these will still go through `Var`)
    case Term.Var(_) => true
    case _ => false
  }

  def hasTailRecursiveCall(rec: CurrentRec, term: Term): Boolean =
    !rec.isEmpty && (term match {
      case Term.Apply(f, args) => f match {
        case Term.Var(v) => rec.contains(v, args.length)
        case _ => false
      }
      case Term.LetRec(bindings, body) =>
        hasTailRecursiveCall(rec.shadow(bindings.map(_._1)), body)
      case Term.Let(bindings, body) =>
        hasTailRecursiveCall(rec.shadow(bindings.map(_._1)), body)
      case Term.If(_, t, f) =>
        hasTailRecursiveCall(rec, t) || hasTailRecursiveCall(rec, f)
      case _ => false
  })

  @inline def evalLam(c: Lambda, r: R, top: StackPtr,
                      stackU: Array[U], x1: U, x0: U,
                      stackB: Array[B], x1b: B, x0b: B): U =
    try c(r, top, stackU, x1, x0, stackB, x1b, x0b)
    catch { case TailCall => loop(r, top, stackU, stackB) }

  @inline def eval(c: Computation, r: R, rec: Lambda, top: StackPtr,
                   stackU: Array[U], x1: U, x0: U,
                   stackB: Array[B], x1b: B, x0b: B): U =
    try c(r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
    catch { case TailCall => loop(r, top, stackU, stackB) }

  def loop(r: R, top: StackPtr, stackU: Array[U], stackB: Array[B]): U = {
    while (true) {
      try {
        // We've just caught a tail call - the arguments for the tail call are
        // in `r`. We copy these arguments to the current stack.
        if (r.stackArgsCount != 0) {
          System.arraycopy(stackU, r.argsStart.toInt, stackU,
                           top.toInt + 1, r.stackArgsCount)
          System.arraycopy(stackB, r.argsStart.toInt, stackB,
                           top.toInt + 1, r.stackArgsCount)
        }
        // ... and then null out the rest of the stack past the last argument
        // (todo: this is correct but maybe excessive - could be lazier or more
        //  targeted about nulling out the stack)
        java.util.Arrays.fill(
          stackB.asInstanceOf[Array[AnyRef]],
          top.toInt + r.stackArgsCount + 1,
          stackB.length, null)

        return r.tailCall(r, top.incBy(r.stackArgsCount), stackU, r.x1, r.x0, stackB, r.x1b, r.x0b)
      }
      catch {
        case TailCall =>
      }
    }
    U0
  }

  abstract class CompiledVar(val position: Int) extends Computation

  case object CompiledVar0 extends CompiledVar(0) {
    def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B) =
      returnBoth(r, x0, x0b)
  }
  case object CompiledVar1 extends CompiledVar(0) {
    def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B) =
      returnBoth(r, x1, x1b)
  }

  def compileVar(e: Term, name: Name, env: Vector[Name], currentRec: CurrentRec): Computation = {
    if (currentRec contains name)
      (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        r.boxed = rec
        U0
      }
    else env.indexOf(name) match {
      case -1 => sys.error("unbound name: " + name)
      case 0 => CompiledVar0
      case 1 => CompiledVar1
      case n => new CompiledVar(n) {
        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B) =
          returnBoth(r, top.u(stackU, n), top.b(stackB, n))
      }
    }
  }

  def compileRef(e: Term, name: Name, env: Vector[Name], currentRec: CurrentRec): ParamLookup = {
    if (currentRec.contains(name)) (rec,_,_,_,_) => rec
    else env.indexOf(name) match {
      case -1 => sys.error("unbound name: " + name)
      case 0 => (_,_,_,_,x0b) => x0b
      case 1 => (_,_,_,x1b,_) => x1b
      case n => (_,top,stackB,_,_) => top.b(stackB, n)
    }
  }

  abstract class ParamLookup {
    def apply(rec: Lambda, top: StackPtr, stackB: Array[B], x1b: B, x0b: B): B
  }

  object CaseNoMatch extends Throwable {
    override def fillInStackTrace(): Throwable = this
  }

  case class MatchFail(originalScrutinee: Term, scrutinee: Term, cases: List[MatchCase[Term]])
    extends Throwable {
    import util.PrettyPrint
    override def fillInStackTrace(): Throwable = this
    override def toString = (
      "match fail: \n" +
        PrettyPrint.prettyTerm(Term.Match(scrutinee)(cases: _*), 0).render(80) + "\n" +
        // Term.Match(scrutinee)(cases: _*) + "\n" +
      "original scrutinee: " +
        PrettyPrint.prettyTerm(originalScrutinee, 0).render(80)
    )
  }

}
