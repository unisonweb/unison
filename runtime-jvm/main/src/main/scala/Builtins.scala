package org.unisonweb

import java.util.function.{LongBinaryOperator, LongUnaryOperator}

import org.unisonweb.Term.{Name, Term}
import org.unisonweb.Value.Lambda
import org.unisonweb.Value.Lambda.Lambda1
import org.unisonweb.compilation._
import org.unisonweb.util.Text.Text
import org.unisonweb.util.{Sequence, Stream, Text}

/* Sketch of convenience functions for constructing builtin functions. */
object Builtins {

  // Stream.empty : Stream a
  val Stream_empty =
    c0l("Stream.empty", Stream.empty[Value])


  // Stream.fromInt : Integer -> Stream Integer
  val Stream_fromInt = // Stream.iterate(unison 0)(Integer_inc)
    fp_l("Stream.fromInt", "n", Stream.fromUnison)

  // Stream.cons : a -> Stream a -> Stream a
  val Stream_cons =
    fpp_l("Stream.cons", "v", "stream",
          (v: Value, stream: Stream[Value]) => v :: stream)

  val Stream_drop =
    fup_l("Stream.drop", "n", "stream",
          (n: Long, s: Stream[Value]) => s.drop(n))

  // Stream.map : Stream a -> (a -> b) -> Stream b
  //  val Stream_map = fpp_p("Stream.map", "stream", "f", (s: Stream[Value], b: Value) => ???)

  val streamBuiltins = Map(
    Stream_empty,
    Stream_fromInt,
    Stream_cons,
    Stream_drop,
  )

  // Sequence.empty : Sequence a
  val Sequence_empty: (Name, Computation) =
    c0("Sequence.empty", Sequence.empty[Value])

  // Sequence.snoc : forall a . Sequence a -> a -> Sequence a
  // Sequence.snoc [] 42
  val Sequence_snoc =
    fpp_p("Sequence.snoc", "seq", "v",
         (seq: Sequence[Value], v: Value) => seq :+ v)

  val Sequence_cons =
    fpp_p("Sequence.cons", "v", "seq",
         (v: Value, seq: Sequence[Value]) => v +: seq)

  val Sequence_take =
    fup_p("Sequence.take", "n", "seq",
         (n: U, seq: Sequence[Value]) => seq take n.toLong)

  val Sequence_size =
    fp_p("Sequence.size", "seq", (seq: Sequence[Value]) => seq.size)

  def c0[A:Decompile](name: String, a: => A)
                     (implicit A: Encode[A]): (Name, Computation.C0) =
    (name, r => A.encode(r, a))

  def c0l[A:NoDecompile](name: String, a: => A): (Name, Computation.C0) =
    (name, r => lazyEncode(r, a, name))

  def termFor(b: (Name, Computation)): Term = Term.Id(b._1)
  def termFor(b: (Name, UnboxedType, Computation)): Term = Term.Id(b._1)
  def computationFor(b: (Name, Computation)): Computation = builtins(b._1)
  def computationFor(b: (Name, UnboxedType, Computation)): Computation =
    builtins(b._1)
  def lambdaFor(b: (Name, Computation)): Lambda =
    computationFor(b) match { case Return(lam: Lambda) => lam }
  def lambdaFor(b: (Name, UnboxedType, Computation)): Lambda =
    computationFor(b) match { case Return(lam: Lambda) => lam }

  //
  // naming convention
  //   - fb_b is a function taking 1 boxed arg, returning 1 boxed result
  //   - fbu_b is a function taking 1 boxed arg, 1 unboxed arg, returning a boxed result
  //   - fuu_u is a function taking 2 unboxed args, returning an unboxed result
  val seqBuiltins = Map(
    // Sequences
    Sequence_empty,
    Sequence_snoc,
    Sequence_cons,
    Sequence_take,
    Sequence_size
  )

  val Integer_inc =
    fu_u("Integer.+", "x", UnboxedType.Integer, _ + 1)

  val Integer_add =
    fuu_u("Integer.+", "x", "y", UnboxedType.Integer, _ + _)

  val Integer_mul =
    fuu_u("Integer.*", "x", "y", UnboxedType.Integer, _ * _)

  val Integer_sub =
    fuu_u("Integer.-", "x", "y", UnboxedType.Integer, _ - _)

  val Integer_div =
    fuu_u("Integer./", "x", "y", UnboxedType.Integer, _ / _)

  val Integer_eq =
    fuu_u("Integer.==", "x", "y", UnboxedType.Boolean,
          (x, y) => boolToUnboxed(x == y))

  val Integer_neq =
    fuu_u("Integer.!=", "x", "y", UnboxedType.Boolean,
          (x, y) => boolToUnboxed(x != y))

  val Integer_lteq =
    fuu_u("Integer.<=", "x", "y", UnboxedType.Boolean,
          (x, y) => boolToUnboxed(x <= y))

  val Integer_gteq =
    fuu_u("Integer.>=", "x", "y", UnboxedType.Boolean,
          (x, y) => boolToUnboxed(x >= y))

  val Integer_lt =
    fuu_u("Integer.<", "x", "y", UnboxedType.Boolean,
          (x, y) => boolToUnboxed(x < y))

  val Integer_gt =
    fuu_u("Integer.>", "x", "y", UnboxedType.Boolean,
          (x, y) => boolToUnboxed(x > y))

  val Integer_signum =
    fu_u("Integer.signum", "x", UnboxedType.Integer, _.signum)

  val Integer_negate =
    fu_u("Integer.negate", "x", UnboxedType.Integer, -_)

  val numericBuiltins = Map(
    // arithmetic
    Integer_add,
    Integer_mul,
    Integer_sub,
    Integer_div,
    Integer_signum,
    Integer_negate,

    // comparison
    Integer_eq,
    Integer_neq,
    Integer_lteq,
    Integer_gteq,
    Integer_lt,
    Integer_gt
  )

  val Boolean_not =
    fu_u("Boolean.not", "b", UnboxedType.Boolean,
         b => boolToUnboxed(b == UFalse))

  val booleanBuiltins = Map(
    Boolean_not,
  )

  val Text_empty =
    c0("Text.empty", util.Text.empty)

  val Text_concatenate =
    fpp_p("Text.concatenate", "textL", "textR", (t1: Text, t2: Text) => t1 ++ t2)

  val Text_take =
    fup_p("Text.take", "codepoint-count", "text", (u: U, t: Text) => t take u)

  val Text_drop =
    fup_p("Text.drop", "codepoint-count", "text", (u: U, t: Text) => t drop u)

  val Text_size =
    fp_p("Text.size", "text", (t: Text) => t.size)

  val Text_eq =
    fpp_u("Text.==", "t1", "t2", UnboxedType.Boolean,
      (t1: Text, t2: Text) => boolToUnboxed(t1 == t2))

  val Text_neq =
    fpp_u("Text.!=", "t1", "t2", UnboxedType.Boolean,
      (t1: Text, t2: Text) => boolToUnboxed(t1 != t2))

  val Text_lteq =
    fpp_u("Text.<=", "t1", "t2", UnboxedType.Boolean,
      (t1: Text, t2: Text) => boolToUnboxed(Text.compare(t1,t2) <= 0))

  val Text_gteq =
    fpp_u("Text.>=", "t1", "t2", UnboxedType.Boolean,
      (t1: Text, t2: Text) => boolToUnboxed(Text.compare(t1,t2) >= 0))

  val Text_lt =
    fpp_u("Text.<", "t1", "t2", UnboxedType.Boolean,
      (t1: Text, t2: Text) => boolToUnboxed(Text.compare(t1,t2) < 0))

  val Text_gt =
    fpp_u("Text.>", "t1", "t2", UnboxedType.Boolean,
      (t1: Text, t2: Text) => boolToUnboxed(Text.compare(t1,t2) > 0))

  val textBuiltins = Map(
    Text_empty,
    Text_concatenate,
    Text_take,
    Text_drop,
    Text_size,
    // todo: indexing, once we add

    Text_eq,
    Text_neq,
    Text_lteq,
    Text_gteq,
    Text_lt,
    Text_gt
  )

  val builtins =
    streamBuiltins ++ seqBuiltins ++ numericBuiltins ++
    booleanBuiltins ++ textBuiltins

  // Polymorphic one-argument function
  def fp_p[A,B](name: Name, arg: Name, f: A => B)
               (implicit A: Decode[A], B: Encode[B]): (Name, Computation) = {
    val body: Computation.C1P = (r,x0,x0b) => B.encode(r, f(A.decode(x0, x0b)))
    val decompile = Term.Id(name)
    name -> Return(new Lambda1(arg, body, None, decompile))
  }

  // Monomorphic one-argument function on unboxed values
  def fu_u(name: Name,
           arg: Name,
           outputType: UnboxedType,
           f: LongUnaryOperator): (Name, Computation) = {
    val body: Computation.C1U = (r,x0) => {
      // Unintuitively, we store the type of the unboxed value in `boxed`
      // since that's a field we don't use for unboxed values.
      r.boxed = outputType
      f.applyAsLong(x0)
    }
    val decompile = Term.Id(name)
    val computation = Return(new Lambda1(arg, body, Some(outputType), decompile))
    name -> computation
  }

  def fp_l[A,B:NoDecompile](name: Name, arg: Name, f: A => B)
                           (implicit A: Decode[A]): (Name, Computation) = {
    val body: Computation.C1P = (r,x0,x0b) =>
      lazyEncode(r, f(A.decode(x0,x0b)), name, Value.fromParam(x0,x0b))
    val lambda = new Lambda.Lambda1(arg, body, None, Term.Id(name))
    name -> Return(lambda)
  }

  def fpp_p[A,B,C](name: Name, arg1: String, arg2: String, f: (A,B) => C)
                  (implicit A: Decode[A],
                            B: Decode[B],
                            C: Encode[C]): (Name, Computation) = {
    val body: Computation.C2P =
      (r,x1,x0,x1b,x0b) =>
        C.encode(r, f(A.decode(x1, x1b), B.decode(x0, x0b)))
    val decompiled = Term.Id(name)
    val lambda = new Lambda.ClosureForming2(arg1, arg2, body, None, decompiled)
    name -> Return(lambda)
  }

  def fpp_l[A,B,C](name: Name, arg1: String, arg2: String, f: (A,B) => C)
                  (implicit A: Decode[A],
                   B: Decode[B],
                   C: NoDecompile[C]): (Name, Computation) = {
    val body: Computation.C2P =
      (r,x1,x0,x1b,x0b) =>
        lazyEncode(r, f(A.decode(x1, x1b), B.decode(x0, x0b)), name,
                   Value.fromParam(x1,x1b),
                   Value.fromParam(x0,x0b))
    val decompiled = Term.Id(name)
    val lambda = new Lambda.ClosureForming2(arg1, arg2, body, None, decompiled)
    name -> Return(lambda)
  }
  abstract class FUP_P[A,B] { def apply(u: U, p: A): B }

  def fup_p[A,B](name: Name, arg1: Name, arg2: Name, f: FUP_P[A,B])
                (implicit A: Decode[A],
                          B: Encode[B]): (Name, Computation) = {
    val body: Computation.C2P = (r,x1,x0,_,x0b) =>
      B.encode(r, f(x1, A.decode(x0, x0b)))
    val decompiled = Term.Id(name)
    val lambda = new Lambda.ClosureForming2(arg1, arg2, body, None, decompiled)
    name -> Return(lambda)
  }

  def fup_l[A,B:NoDecompile](name: Name, arg1: Name, arg2: Name, f: FUP_P[A,B])
                (implicit A: Decode[A]): (Name, Computation) = {
    val body: Computation.C2P = (r,x1,x0,x1b,x0b) =>
      lazyEncode(r, f(x1, A.decode(x0, x0b)), name,
                 Value.fromParam(x1, x1b),
                 Value.fromParam(x0, x0b))

    val decompiled = Term.Id(name)
    val lambda = new Lambda.ClosureForming2(arg1, arg2, body, None, decompiled)
    name -> Return(lambda)
  }

  abstract class FPP_U[A,B] { def apply(a: A, b: B): U }

  def fpp_u[A,B,C](name: Name, arg1: String, arg2: String,
                   outputType: UnboxedType, f: FPP_U[A,B])
                  (implicit A: Decode[A],
                            B: Decode[B]): (Name, Computation) = {
    val body: Computation =
      (r,_,_,_,x1,x0,_,x1b,x0b) => {
        r.boxed = outputType
        f(A.decode(x1, x1b), B.decode(x0, x0b))
      }
    val decompiled = Term.Id(name)
    val lambda = new Lambda.ClosureForming2(arg1, arg2, body, Some(outputType), decompiled)
    name -> Return(lambda)
  }

  def fuu_u(name: Name,
            arg1: Name,
            arg2: Name,
            outputType: UnboxedType,
            f: LongBinaryOperator): (Name, Computation) = {
    val body: Computation.C2U = (r,x1,x0) => {
      r.boxed = outputType
      f.applyAsLong(x1, x0)
    }

    val decompiled = Term.Id(name)

    val lam = new Lambda(2, body, Some(outputType), decompiled) { self =>
      def names = List(arg1, arg2)
      override def saturatedNonTailCall(args: List[Computation]) = args match {
        case List(Return(Value.Unboxed(n1, _)),
                  Return(Value.Unboxed(n2, _))) =>
          val n3 = f.applyAsLong(n1,n2) // constant fold
          val c : Computation.C0 = r => { r.boxed = outputType; n3 }
          c
        case List(CompiledVar0,Return(Value.Unboxed(n, _))) =>
          val c : Computation.C1U = (r,x0) => {
            r.boxed = outputType
            f.applyAsLong(x0, n)
          }
          c
        case List(CompiledVar1,Return(Value.Unboxed(n, _))) =>
          val c : Computation.C2U = (r,x1,_) => {
            r.boxed = outputType
            f.applyAsLong(x1, n)
          }
          c
        case List(Return(Value.Unboxed(n, _)), CompiledVar0) =>
          val c: Computation.C1U = (r,x0) => {
            r.boxed = outputType
            f.applyAsLong(n,x0)
          }
          c
        case List(Return(Value.Unboxed(n, _)), CompiledVar1) =>
          val c: Computation.C2U = (r,x1,_) => {
            r.boxed = outputType
            f.applyAsLong(n,x1)
          }
          c
        case List(CompiledVar1,CompiledVar0) =>
          val c: Computation.C2U = (r,x1,x0) => {
            r.boxed = outputType
            f.applyAsLong(x1,x0)
          }
          c
        case List(CompiledVar0,CompiledVar1) =>
          val c: Computation.C2U = (r,x1,x0) => {
            r.boxed = outputType
            f.applyAsLong(x0,x1)
          }
          c
        case List(arg1: Computation.C2U, arg2: Computation.C2U) =>
          val c: Computation.C2U = (r,x1,x0) => {
            val x1v = arg1(r, x1, x0)
            val x0v = arg2(r, x1, x0)
            r.boxed = outputType
            f.applyAsLong(x1v, x0v)
          }
          c
        case List(arg1,arg2) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          val x1v = eval(arg1,r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
          val x0v = eval(arg2,r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
          r.boxed = outputType
          f.applyAsLong(x1v, x0v)
        }
      }
      override def underapply(builtins: Environment)
                             (argCount: Int, substs: Map[Name, Term]): Lambda =
        substs.toList match {
          case List((_,term)) => term match {
            case Term.Compiled(p: Param) =>
              // cast is okay, because in `fuu_u`, args are Unboxed.
              val n = p.toValue.asInstanceOf[Value.Unboxed].n
              val body: Computation =
                (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
                  r.boxed = outputType
                  f.applyAsLong(n, x0)
                }
              new Lambda(1, body, unboxedType, Term.Apply(decompiled, term)) {
                def names = self.names drop argCount
              }
            case _ => sys.error("")
          }
          case _ => sys.error("unpossible")
        }
    }
    name -> Return(lam)
  }

  trait Decode[+T] { def decode(u: U, b: B): T }
  object Decode extends LowPriorityDecode {
    implicit val decodeValue: Decode[Value] = (u, v) => Value.fromParam(u, v)
    implicit val decodeLong: Decode[Long] = (u,_) => u
    implicit val decodeDouble: Decode[Double] = (u,_) => unboxedToDouble(u)

// TODO: If we include this implicit, it gets selected, even if the function
// is just asking for a `Decode[Value]`.
// See https://issues.scala-lang.org/browse/SI-2509
//
//    implicit val decodeLambda: Decode[Lambda] =
//      (_, b) => b.toValue.asInstanceOf[Lambda]
  }
  trait LowPriorityDecode {
    implicit def decodeAssumeExternal[A]: Decode[A] =
      (_, b) => b.toValue.asInstanceOf[External].get.asInstanceOf[A]
  }

  /** Encode a scala type `A` in `Result => U` form. */
  trait Encode[-A] { def encode(r: Result, a: A): U }
  object Encode {
    implicit def encodeDecompilableExternal[A:Decompile]: Encode[A] =
      (r, a) => {
        r.boxed = External(a)
        U0
      }

    implicit val encodeLong: Encode[Long] =
      (r, a) => { r.boxed = UnboxedType.Integer; a }
    implicit val encodeInt: Encode[Int] =
      (r, a) => { r.boxed = UnboxedType.Integer; a.toLong }
    implicit val encodeDouble: Encode[Double] =
      (r, a) => { r.boxed = UnboxedType.Float; doubleToUnboxed(a) }
  }

  final class NoDecompile[A] private()
  object NoDecompile {
    implicit val noDecompileStreamBoxed: NoDecompile[Stream[Value]] = null
    implicit val noDecompileStreamUnboxed: NoDecompile[Stream[UnboxedType]] = null
  }

  trait Decompile[A] { def decompile(a: A): Term }

  object Decompile {
    implicit val decompileSequence: Decompile[Sequence[Value]] =
      s => Term.Sequence(s map (_.decompile))
    implicit val decompileText: Decompile[Text] =
      Term.Text(_)
    implicit val decompileUnit: Decompile[Unit] =
      u => BuiltinTypes.Unit.term
    implicit def decompilePair[A,B](implicit A: Decompile[A], B: Decompile[B]): Decompile[(A,B)] =
      p => BuiltinTypes.Tuple.term(A.decompile(p._1), B.decompile(p._2))
  }

  def lazyEncode[A:NoDecompile](r: Result, a: A, fn: Name, args: Value*): U = {
    r.boxed = External(a, Term.Apply(Term.Id(fn),
                                     args.map(Term.Compiled.apply): _*))
    U0
  }

  sealed abstract class Extract[A, B] {
    val extract: FUP_P[B,A]
    def toBoxed(a: A): B
    def toUnboxed(a: A): U
  }

  abstract class External(val get: Any) extends Value
  object External {
    def apply[A](value: A, decompiled: Term): Value =
      new External(value) { def decompile = decompiled }
    def apply[A](value: A)(implicit A: Decompile[A]) =
      new External(value) {
        def decompile: Term = A.decompile(value)
      }
  }
}


