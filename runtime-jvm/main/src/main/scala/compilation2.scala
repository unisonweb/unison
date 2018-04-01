package org.unisonweb

import org.unisonweb.Term.{Name, Term}
import org.unisonweb.compilation2.Value.Lambda

object compilation2 {

  type U = Double // unboxed values
  val U0: U = 0
  val True: U = 1
  val False: U = 0
  type B = Param // boxed values
  type R = Result
  type Arity = Int
  type IsTail = Boolean
  val IsTail = true
  val IsNotTail = false

  val K = 2

  case class CurrentRec(get: Option[(Name, Arity)]) extends AnyVal {
    def isEmpty = get.isEmpty
    def contains(name: Name): Boolean = get.exists(_._1 == name)
    def contains(name: Name, arity: Arity): Boolean = get == Some((name, arity))
    /** knock out the currentRec if appropriate; if it is shadowed, it won't be called */
    def shadow(name: Name): CurrentRec = CurrentRec(get.filterNot(name == _._1))
    /** knock out the currentRec if appropriate; if it is shadowed, it won't be called */
    def shadow(names: Seq[Name]): CurrentRec = CurrentRec(get.filterNot(names contains _._1))
  }
  object CurrentRec {
    def none = CurrentRec(None)
    def apply(name: Name, arity: Arity): CurrentRec = CurrentRec(Some((name,arity)))
  }

  case class RecursiveVars(get: Set[Name]) extends AnyVal {
    def contains(name: Name): Boolean = get.contains(name)
    def +(name: Name): RecursiveVars = RecursiveVars(get + name)
    def ++(names: Seq[Name]): RecursiveVars = RecursiveVars(get ++ names)
    def -(name: Name): RecursiveVars = RecursiveVars(get.filterNot(name == _))
    def --(names: Seq[Name]): RecursiveVars = RecursiveVars(get.filterNot(names contains _))
  }
  object RecursiveVars {
    def empty = RecursiveVars(Set())
  }

  // type StackPtr = Int
  class StackPtr(private val top: Int) extends AnyVal {
    def toInt = top
    @inline def u(stackU: Array[U], envIndex: Int): U =
      stackU(top - envIndex + K)
    @inline def b(stackB: Array[B], envIndex: Int): B =
      stackB(top - envIndex + K)
    @inline def increment(by: Int) = {
      assert(by >= 0)
      new StackPtr(top+by)
    }
    @inline def pushU(stackU: Array[U], i: Int, u: U): Unit =
      stackU(top + i + 1) = u
    @inline def pushB(stackB: Array[B], i: Int, b: B): Unit =
      stackB(top + i + 1) = b
  }
  object StackPtr {
    def empty = new StackPtr(-1)
  }

  abstract class Param {
    def toValue: Value
    def isRef: Boolean = false
  }
  object Param {
    def apply(u: U, b: B): Param = if (b eq null) Value.Num(u) else b
  }

  class Ref(val name: Name, var value: Value) extends Param {
    def toValue = value
    override def isRef = true
  }

  abstract class Value extends Param {
    def toValue = this
    def decompile: Term
    def toResult(r: Result): U
  }
  object Value {
    def apply(u: U, b: Value): Value = if (b eq null) Num(u) else b

    case class Num(n: U) extends Value {
      def decompile = Term.Num(n)
      def toResult(r: Result) = n
    }

    abstract class Lambda(final val arity: Int, final private val body: Computation, val decompile: Term) extends Value {
      def names: List[Name]
      def toComputation = Return(this, decompile)

      final def apply(r: R, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U =
        body(r, this, top, stackU, x1, x0, stackB, x1b, x0b)

      def toResult(r: Result) = { r.boxed = this; U0 }

      def saturatedNonTailCall(args: List[Computation]): Computation =
        compileStaticFullySaturatedNontailCall(this, args)

      def underapply(builtins: Name => Computation)(argCount: Int, substs: Map[Name, Term]): Value.Lambda = decompile match {
        case Term.Lam(names, body) =>
          compile(builtins)(Term.Lam(names drop argCount: _*)(ABT.substs(substs)(body)), Vector.empty, CurrentRec.none, RecursiveVars.empty, IsNotTail) match {
            case Return(v: Value.Lambda) => v
            case c => sys.error("compiling a closed Term.Lambda failed to produce a Value.Lambda: " + c)
          }
      }
    }
    object Lambda {
      def apply(arity: Int, body: Computation, decompile: Term) =
        new Lambda(arity, body, decompile) {
          val names = decompile match { case Term.Lam(names, _) => names }
        }

      def unapply(l: Lambda): Option[(Int, Computation, Term)] =
        Some((l.arity, l.body, l.decompile))
    }

  }

  case object SelfCall extends Throwable { override def fillInStackTrace = this }
  case object TailCall extends Throwable { override def fillInStackTrace = this }


  case class Result(
    var boxed: Value = null,
    var tailCall: Lambda = null,
    var argsStart: StackPtr = new StackPtr(0),
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
  abstract class Computation {
    def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U
  }

  /** Put `v` into `r.boxed` */
  @inline private def returnBoxed(r: R, v: Value): U = { r.boxed = v; U0 }

  // todo: think through whether can elide
  @inline private def returnUnboxed(r: R, unboxed: U): U = { r.boxed = null; unboxed }

  @inline private def returnBoth(r: R, x0: U, x0b: B) = { if (x0b ne null) r.boxed = x0b.toValue; x0 }


  case class Self(name: Name) extends Computation {
    def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U =
      returnBoxed(r, rec)
  }

  // todo: maybe opportunities for more expressive matching by breaking this into sub-cases
  abstract class Return(e: Term) extends Computation {
    def value: Value
  }

  object Return {
    def apply(v: Value, t: Term): Computation = v match {
      case Value.Num(d) => compileNum(d)
      case f@Value.Lambda(_, _, _) => new Return(f.decompile) {
        def value: Value = f

        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U =
          returnBoxed(r, f)
      }
    }
    def unapply(c: Computation): Option[Value] = {
      c match {
        case r: Return => Some(r.value)
        case _ => None
      }
    }
  }

  def compileNum(n: U): Computation = new Return(Term.Num(n)) {
    def value = Value.Num(n)
    def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B): U =
      returnUnboxed(r, n)
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

  def compileStaticFullySaturatedTailCall(lam: Lambda, compiledArgs: List[Computation]): Computation =
    compiledArgs match {
      case List(arg) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) =>
        doTailCall(lam, arg, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
      case List(arg1, arg2) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) =>
        doTailCall(lam, arg1, arg2, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
      case args =>
        val argsArray = args.toArray
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) =>
          doTailCall(lam, argsArray, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
    }

  def compileFullySaturatedSelfTailCall(compiledArgs: List[Computation]): Computation =
    compiledArgs match {
      case List(arg) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        val rx0v = eval(arg, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        r.x0 = rx0v
        r.x0b = r.boxed
        // we don't need to set r.stackArgsCount;
        // it's known in the self-calling function
        throw SelfCall
      }
      case List(arg1, arg2) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        val arg1v = eval(arg1, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        val arg1vb = r.boxed
        val rx0v = eval(arg2, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
        r.x0 = rx0v
        r.x0b = r.boxed
        r.x1 = arg1v
        r.x1b = arg1vb
        // we don't need to set r.stackArgsCount
        throw SelfCall
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

          val arg1v = eval(argsArray(argsArray.length-2), r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          val arg1vb = r.boxed
          val rx0v = eval(argsArray(argsArray.length - 1), r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          r.x0 = rx0v
          r.x0b = r.boxed
          r.x1 = arg1v
          r.x1b = arg1vb
          throw SelfCall
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

  def compileUnderappliedCall(builtins: Name => Computation)(
    lam: Lambda, compiledArgs: Array[Computation]): Computation = {

    val names = lam.names.toArray
    val argCount = compiledArgs.length
    val remNames = names drop argCount
    assert(argCount < lam.arity)
    (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
      @inline @annotation.tailrec def go(i: Int, substs: Map[Name, Term]): Map[Name, Term] = {
        if (i < argCount) {
          val argv = eval(compiledArgs(i), r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          val param = Term.Compiled2(Param(argv, r.boxed))
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
    r.argsStart = top.increment(1)
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
    fn(r, top.increment(args.length - K), stackU, argv1, argv2, stackB, argv1b, argv2b)
  }

  // in `f x`
  // - evaluate `f`
  // - notice `f` has arity 1, and that the call is in tail position
  //    - issue tail call to `f`
  // - if `f` has arity > 1
  def dynamicCall(builtins: Name => Computation)(
                  fn: Computation, args: List[Computation], isTail: Boolean): Computation = {
    val argsArray = args.toArray
    (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
      @annotation.tailrec
      def invokeDynamic(fn: Lambda, args: Array[Computation]): U = {
        if (args.length == fn.arity)
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

  def compileLambda(builtins: Name => Computation)
                   (e: Term, env: Vector[Name],
                    currentRec: CurrentRec, recVars: RecursiveVars,
                    names: List[Name], body: Term): Computation = {

    val freeVars = Term.freeVars(e)
    // The lambda is closed
    if (freeVars.isEmpty) {
      val cbody = compile(builtins)(body, names.reverse.toVector,
        currentRec.shadow(names), RecursiveVars.empty, IsTail)
      Return(Lambda(names.length, cbody, e), e)
    }
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
        val evaledFreeVars: Map[Name, Term] = compiledFrees.mapValues {
          c =>
            val evaluatedVar = c(r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
            val value = Value(evaluatedVar, r.boxed)
            Term.Compiled2(value)
        }

        val evaledRecVars: Map[Name, Term] = compiledFreeRecs.transform {
          (name, lookup) =>
            if (currentRec.contains(name)) Term.Self(name)
            else {
              val evaluatedVar = lookup(rec, top, stackB, x1b, x0b)
              if (evaluatedVar eq null) sys.error(name + " refers to null stack slot.")
              require(evaluatedVar.isRef)
              Term.Compiled2(evaluatedVar)
            }
        }

        val lam2 = Term.Lam(names: _*)(
          body = ABT.substs(evaledFreeVars ++ evaledRecVars)(body)
        )
        assert(Term.freeVars(lam2).isEmpty)
        r.boxed = compile(builtins)(
          lam2, Vector(), currentRec.shadow(names), RecursiveVars.empty, IsNotTail
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

  def normalize(builtins: Name => Computation)(e: Term): Term = {
    val c = compileTop(builtins)(e)
    val v = run(c)
    val x = Term.etaNormalForm(v.decompile)
    Term.fullyDecompile2(x)
  }

  def run(c: Computation): Value = {
    val r = Result()
    val us = new Array[U](1024)
    val bs = new Array[B](1024)
    val cc = eval(c, r, null, new StackPtr(-1), us, U0, U0, bs, null, null)
    Value(cc, r.boxed)
  }

  /** Compile top-level term */
  def compileTop(builtins: Name => Computation)(e: Term) =
    compile(builtins)(e, Vector(), CurrentRec.none, RecursiveVars.empty, IsTail)

  def compile(builtins: Name => Computation)(
    e: Term,
    env: Vector[Name] = Vector.empty,
    currentRec: CurrentRec = CurrentRec.none,
    recVars: RecursiveVars = RecursiveVars.empty,
    isTail: IsTail): Computation = {

    e match {
      case Term.Num(n) => compileNum(n)
      case Term.Builtin(name) => builtins(name)
      case Term.Compiled2(param) =>
        if (param.toValue eq null)
          (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => param.toValue.toResult(r)
        else Return(param.toValue, e)
      case Term.Self(name) => new Self(name)
      case Term.Var(name) => compileVar(e, name, env, currentRec)
      case Term.If(cond, t, f) =>
        val ccond = compile(builtins)(cond, env, currentRec, recVars, IsNotTail)
        val ct = compile(builtins)(t, env, currentRec, recVars, isTail)
        val cf = compile(builtins)(f, env, currentRec, recVars, isTail)
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) =>
          if (eval(ccond, r, rec, top, stackU, x1, x0, stackB, x1b, x0b) != U0)
            ct(r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          else
            cf(r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
      case Term.Let1(name, b, body) =>
        val cb = compile(builtins)(b, env, currentRec, recVars, IsNotTail)
        val cbody = compile(builtins)(body, name +: env, currentRec.shadow(name),
          recVars - name, isTail)
        val push = compilation2.push(env, Term.freeVars(body)).push1
        (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          val rb = eval(cb, r, rec, top, stackU, x1, x0, stackB, x1b, x0b)
          val rbb = r.boxed
          push(top, stackU, stackB, x1, x1b)
          cbody(r, rec, top.increment(1), stackU, x0, rb, stackB, x0b, rbb)
        }
      // todo: Let2, etc.

      case lam@Term.Lam(names, body) =>
        val compiledLambda: Computation =
          compileLambda(builtins)(e, env, currentRec, recVars, names, body)

        if (hasTailRecursiveCall(currentRec.shadow(names), body)) {
          // let rec
          //   go n = if n == 0 then n else go (n - 1)
          //   go
          // gets converted to:
          // let rec
          //   go-inner n = if n == 0 then n else throw SelfCall (n - 1)
          //   go n = while (true) return { try go-inner n catch { case SelfCall n2 => go-inner n2 }}
          //   go

          def handleSelfCalls(innerLambda: Lambda): Lambda = {
            assert(names.length == innerLambda.arity)
            val needsCopy = names.length > 2
            // inner lambda may throw SelfCall, so we create wrapper Lambda
            // to process those
            val outerLambdaBody: Computation = (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
              val stackArgsCount = innerLambda.arity - K
              val newArgsSrcIndex = top.increment(stackArgsCount).toInt //
              val newArgsDestIndex = top.toInt

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
                  case SelfCall =>
                    if (needsCopy) {
                      System.arraycopy(
                        stackU, newArgsSrcIndex,
                        stackU, newArgsDestIndex,
                        stackArgsCount
                      )
                      System.arraycopy(
                        stackB, newArgsSrcIndex,
                        stackB, newArgsDestIndex,
                        stackArgsCount
                      )
                    }
                }
                go(r.x1, r.x0, r.x1b, r.x0b)
              }

              go(x1, x0, x1b, x0b)
            }
            Lambda(names.length, outerLambdaBody, innerLambda.decompile)
          }

          compiledLambda match {
            case Return(innerLambda: Lambda) =>
              Return(handleSelfCalls(innerLambda), e)

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
              if (isTail && needsTailCall(fn)) compileStaticFullySaturatedTailCall(lam, compiledArgs)

              // static non-tail call, fully saturated
              //   ex: let x = (x -> x + 1) 1; x
              //               ^^^^^^^^^^^^^^
              else lam.saturatedNonTailCall(compiledArgs)

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
            case Term.Self(_) | Term.Var(_) =>
              val name = fn match { case Term.Self(name) => name; case Term.Var(name) => name }
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
          case (name, lam @ Term.Lam(names, _)) =>
            compile(builtins)(lam, env2, CurrentRec(name, arity = names.length),
              recVars2, IsNotTail)
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
            val push = compilation2.push(env, Term.freeVars(e)).push1
            (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
              var bindingResult = new Ref(name, null)
              push(top, stackU, stackB, x1, x1b)
              val v = eval(cbinding, r, rec, top.increment(1),
                           stackU, x0, U0,
                           stackB, x0b, bindingResult)
              bindingResult.value = Value(v, r.boxed)
              cbody(r, rec, top.increment(1), stackU, x0, U0, stackB, x0b, bindingResult)
            }
          case Array(cbinding1, cbinding0) =>
            val name1 = bindingNames(0)
            val name0 = bindingNames(1)
            val push = compilation2.push(env, Term.freeVars(e)).push2
            (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
              val r1 = new Ref(name1, null)
              val r0 = new Ref(name0, null)
              push(top, stackU, stackB, x1, x1b, x0, x0b)
              val top2 = top.increment(2)
              val r1v = eval(cbinding1, r, rec, top2,
                             stackU, U0, U0, stackB, r1, r0)
              r1.value = Value(r1v, r.boxed)
              val r0v = eval(cbinding0, r, rec, top2,
                             stackU, U0, U0, stackB, r1, r0)
              r0.value = Value(r0v, r.boxed)
              cbody(r, rec, top2, stackU, U0, U0, stackB, r1, r0)
            }
          case cbindings =>
            val push = compilation2.push(env, Term.freeVars(e)).push2
            assert(K == 2)
            (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
              // todo: can this be faster?
              val bindingResults = bindingNames.map(name => new Ref(name, null))
              push(top, stackU, stackB, x1, x1b, x0, x0b)
              val top2 = top.increment(2)
              @inline @annotation.tailrec def pushRefs(i: Int): Unit = {
                if (i < cbindings.length - 2) {
                  top2.pushU(stackU, i, U0)
                  top2.pushB(stackB, i, bindingResults(i))
                  pushRefs(i+1)
                }
              }

              // push n - K more binding results, and then put the last
              // K binding results into registers
              // where K must be 2
              pushRefs(0)
              val topN = top.increment(cbindings.length)
              val brx1 = bindingResults(bindingResults.length - 2)
              val brx0 = bindingResults(bindingResults.length - 1)
              @inline @annotation.tailrec def evalBindings(i: Int): Unit = {
                if (i < cbindings.length) {
                  val v = eval(cbindings(i), r, rec, topN,
                               stackU, U0, U0, stackB, brx1, brx0)
                  bindingResults(i).value = Value(v, r.boxed)
                  evalBindings(i+1)
                }
              }
              evalBindings(0)

              cbody(r, rec, topN, stackU, U0, U0, stackB, brx1, brx0)
            }
        }
    }
  }

  /** Returns `true` if a fully saturated call of `fn` should be compiled as a tail call. */
  def needsTailCall(fn: Term): Boolean = fn match {
    case Term.Var(_) => true
    case _ => false
  }

  def hasTailRecursiveCall(rec: CurrentRec, term: Term): Boolean =
    !rec.isEmpty && (term match {
      case Term.Apply(f, args) => f match {
        case Term.Var(v) => rec.contains(v, args.length)
        case Term.Self(v) => rec.contains(v, args.length)
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

        return r.tailCall(r, top.increment(r.stackArgsCount), stackU, r.x1, r.x0, stackB, r.x1b, r.x0b)
      }
      catch {
        case TailCall =>
      }
    }
    U0
  }

  abstract class CompiledVar(val position: Int) extends Computation

  case object CompiledVar0 extends CompiledVar(0) {
    def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B) = returnBoth(r, x0, x0b)
  }
  case object CompiledVar1 extends CompiledVar(0) {
    def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B) = returnBoth(r, x1, x1b)
  }

  def compileVar(e: Term, name: Name, env: Vector[Name], currentRec: CurrentRec): Computation =
    if (currentRec.contains(name)) new Self(name)
    else env.indexOf(name) match {
      case -1 => sys.error("unbound name: " + name)
      case 0 => CompiledVar0
      case 1 => CompiledVar1
      case n => new CompiledVar(n) {
        def apply(r: R, rec: Lambda, top: StackPtr, stackU: Array[U], x1: U, x0: U, stackB: Array[B], x1b: B, x0b: B) =
          returnBoth(r, top.u(stackU, n), top.b(stackB, n))
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
}
