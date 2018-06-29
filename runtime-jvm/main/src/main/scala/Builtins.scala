package org.unisonweb

import java.util.function.{DoubleBinaryOperator, LongBinaryOperator, LongPredicate, LongUnaryOperator}

import org.unisonweb.Term.{Name, Term}
import org.unisonweb.Value.Lambda
import org.unisonweb.Value.Lambda.Lambda1
import org.unisonweb.compilation._
import org.unisonweb.util.Text.Text
import org.unisonweb.util.{Sequence, Stream, Text}

/* Sketch of convenience functions for constructing builtin functions. */
object Builtins {
  def env =
    (new Array[U](20), new Array[B](20), StackPtr.empty, Result())

  // Stream.empty : Stream a
  val Stream_empty =
    c0z("Stream.empty", Stream.empty[Value])


  // Stream.fromInt : Integer -> Stream Integer
  val Stream_fromInt = // Stream.iterate(unison 0)(Integer_inc)
    fp_z("Stream.from-int64", "n", Stream.fromUnison)

  // Stream.cons : a -> Stream a -> Stream a
  val Stream_cons =
    fpp_z("Stream.cons", "v", "stream",
          (v: Value, stream: Stream[Value]) => v :: stream)

  val Stream_take =
    flp_z("Stream.take", "n", "stream",
          (n, s: Stream[Value]) => s.take(n))

  val Stream_drop =
    flp_z("Stream.drop", "n", "stream",
          (n, s: Stream[Value]) => s.drop(n))

  // Stream.map : (a -> b) -> Stream a -> Stream b
  val Stream_map =
    fpp_z("Stream.map", "f", "stream",
          (f: Value, s: Stream[Value]) =>
            s.map(UnisonToScala.toUnboxed1(f.asInstanceOf[Lambda])(env)))

  // Stream.foldLeft : b -> (b -> a -> b) -> Stream a -> b
  val Stream_foldLeft =
    fppp_p("Stream.fold-left", "acc", "f", "stream",
           (acc: Value, f: Value, s: Stream[Value]) =>
             s.foldLeft(acc)(
               UnisonToScala.toUnboxed2(f.asInstanceOf[Lambda])(env))
           // todo: env needs to be bigger here
    )

  abstract class FPPP_P[A,B,C,D] { def apply(a: A, b: B, c: C): D }
  def fppp_p[A,B,C,D](name: Name, arg1: Name, arg2: Name, arg3: Name,
                      f: FPPP_P[A,B,C,D])
                     (implicit
                      A: Decode[A],
                      B: Decode[B],
                      C: Decode[C],
                      D: Encode[D]): (Name, Computation) = {
    val body: Computation =
      (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        val x2 = top.u(stackU, 2)
        val x2b = top.b(stackB, 2)
        D.encode(r, f(
          A.decode(x2, x2b),
          B.decode(x1, x1b),
          C.decode(x0, x0b)
        ))
      }
    val decompiled = Term.Id(name)
    val lambda =
      new Value.Lambda.ClosureForming(List(arg1, arg2, arg3), body, decompiled)
    name -> Return(lambda)
  }

  val streamBuiltins = Map(
    Stream_empty,
    Stream_fromInt,
    Stream_cons,
    Stream_drop,
    Stream_take,
    Stream_map,
    Stream_foldLeft,
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
    flp_p("Sequence.take", "n", "seq", (n, seq: Sequence[Value]) => seq take n)

  val Sequence_size =
    fp_p("Sequence.size", "seq", (seq: Sequence[Value]) => Unsigned(seq.size))

  def c0[A:Decompile](name: String, a: => A)
                     (implicit A: Encode[A]): (Name, Computation.C0) =
    (name, r => A.encode(r, a))

  def c0z[A](name: String, a: => A)(implicit A: LazyEncode[A]): (Name, Computation.C0) =
    (name, r => A.encodeOp(r, a, name))

  def termFor(b: (Name, Computation)): Term = Term.Id(b._1)
  def termFor(b: (Name, UnboxedType, Computation)): Term = Term.Id(b._1)
  def computationFor(b: (Name, Computation)): Computation = builtins(b._1)
  def lambdaFor(b: (Name, Computation)): Lambda =
    computationFor(b) match { case Return(lam: Lambda) => lam }

  case class Unsigned(raw: Long) extends AnyVal

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

  // Signed machine integers
  val Int64_inc =
    fl_l("Int64.increment", "x", _ + 1)

  val Int64_isEven =
    fl_b("Int64.isEven", "x", _ % 2 == 0)

  val Int64_isOdd =
    fl_b("Int64.isOdd", "x", _ % 2 != 0)

  val Int64_add =
    fll_l("Int64.+", "x", "y", _ + _)

  val Int64_mul =
    fll_l("Int64.*", "x", "y", _ * _)

  val Int64_sub =
    fll_l("Int64.-", "x", "y", _ - _)

  val Int64_div =
    fll_l("Int64./", "x", "y", _ / _)

  val Int64_eq =
    fll_b("Int64.==", "x", "y", _ == _)

  val Int64_neq =
    fll_b("Int64.!=", "x", "y", _ != _)

  val Int64_lteq =
    fll_b("Int64.<=", "x", "y", _ <= _)

  val Int64_gteq =
    fll_b("Int64.>=", "x", "y", _ >= _)

  val Int64_lt =
    fll_b("Int64.<", "x", "y", _ < _)

  val Int64_gt =
    fll_b("Int64.>", "x", "y", _ > _)

  val Int64_signum =
    fl_l("Int64.signum", "x", _.signum)

  val Int64_negate =
    fl_l("Int64.negate", "x", -_)

  // Unsigned machine integers
  def uint(n: Long): Term = Term.Unboxed(longToUnboxed(n), UnboxedType.UInt64)

  val UInt64_toInt64 =
    fl_l("UInt64.toInt64", "x", x => x)

  val UInt64_inc =
    fn_n("UInt64.increment", "x", _ + 1)

  val UInt64_isEven =
    fl_b("UInt64.isEven", "x", _ % 2 == 0)

  val UInt64_isOdd =
    fl_b("UInt64.isOdd", "x", _ % 2 != 0)

  val UInt64_add =
    fnn_n("UInt64.+", "x", "y", _ + _)

  val UInt64_mul =
    fnn_n("UInt64.*", "x", "y", _ * _)

  val UInt64_drop =
    fnn_n("UInt64.drop", "x", "y", (x, y) =>
      if (x < 0 && y < 0)
        if (x >= y) x - y else 0
      else if (x < 0 && y >= 0) x - y
      else if (x >= 0 && y < 0) 0
      else x - y max 0
  )

  val UInt64_sub =
    fnn_n("UInt64.sub", "x", "y", (x, y) => x - y)

  val UInt64_div =
    fnn_n("UInt64./", "x", "y", java.lang.Long.divideUnsigned(_,_))

  val UInt64_eq =
    fll_b("UInt64.==", "x", "y", _ == _)

  val UInt64_neq =
    fll_b("UInt64.!=", "x", "y", _ != _)

  val UInt64_lteq =
    fll_b("UInt64.<=", "x", "y", (x, y) =>
      java.lang.Long.compareUnsigned(x,y) <= 0
    )

  val UInt64_gteq =
    fll_b("UInt64.>=", "x", "y", (x, y) =>
      java.lang.Long.compareUnsigned(x,y) >= 0
    )

  val UInt64_lt =
    fll_b("UInt64.<", "x", "y", (x, y) =>
      java.lang.Long.compareUnsigned(x,y) < 0
    )

  val UInt64_gt =
    fll_b("UInt64.>", "x", "y", (x, y) =>
      java.lang.Long.compareUnsigned(x,y) > 0
    )

  // 64-bit floating point numbers
  def float(d: Double): Term =
    Term.Unboxed(doubleToUnboxed(d), UnboxedType.Float)

  val Float_add = fdd_d("Float.+", "x", "y", _ + _)

  val Float_sub = fdd_d("Float.-", "x", "y", _ - _)

  val Float_mul = fdd_d("Float.*", "x", "y", _ * _)

  val Float_div = fdd_d("Float./", "x", "y", _ / _)

  val Float_eq = fdd_b("Float.==", "x", "y", _ == _)

  val Float_neq = fdd_b("Float.!=", "x", "y", _ != _)

  val Float_lteq = fdd_b("Float.<=", "x", "y", _ <= _)

  val Float_gteq = fdd_b("Float.>=", "x", "y", _ >= _)

  val Float_gt = fdd_b("Float.>", "x", "y", _ > _)

  val Float_lt = fdd_b("Float.<", "x", "y", _ < _)

  val numericBuiltins: Map[Name, Computation] = Map(
    // arithmetic
    Int64_inc,
    Int64_add,
    Int64_sub,
    Int64_mul,
    Int64_div,
    Int64_signum,
    Int64_negate,

    UInt64_toInt64,
    UInt64_inc,
    UInt64_isEven,
    UInt64_isOdd,
    UInt64_mul,
    UInt64_drop,
    UInt64_add,
    UInt64_sub,
    UInt64_div,

    Float_add,
    Float_sub,
    Float_mul,
    Float_div,

    // comparison
    Int64_eq,
    Int64_neq,
    Int64_lteq,
    Int64_gteq,
    Int64_lt,
    Int64_gt,
    UInt64_eq,
    UInt64_neq,
    UInt64_lteq,
    UInt64_gteq,
    UInt64_lt,
    UInt64_gt,
    Float_eq,
    Float_neq,
    Float_lteq,
    Float_gteq,
    Float_lt,
    Float_gt
  )

  val Boolean_not =
    fb_b("Boolean.not", "b", !_)

  val booleanBuiltins = Map(
    Boolean_not,
  )

  val Text_empty =
    c0("Text.empty", util.Text.empty)

  val Text_concatenate =
    fpp_p("Text.concatenate", "textL", "textR", (t1: Text, t2: Text) => t1 ++ t2)

  val Text_take =
    flp_p("Text.take", "codepoint-count", "text",
          (codepointCount, t: Text) => t take codepointCount)

  val Text_drop =
    flp_p("Text.drop", "codepoint-count", "text",
          (codepointCount, t: Text) => t drop codepointCount)

  val Text_size =
    fp_p("Text.size", "text", (t: Text) => Unsigned(t.size))

  val Text_eq =
    fpp_b[Text,Text]("Text.==", "t1", "t2", _ == _)

  val Text_neq =
    fpp_b[Text,Text]("Text.!=", "t1", "t2", _ != _)

  val Text_lteq =
    fpp_b[Text,Text]("Text.<=", "t1", "t2", Text.compare(_,_) <= 0)

  val Text_gteq =
    fpp_b[Text,Text]("Text.>=", "t1", "t2", Text.compare(_,_) >= 0)

  val Text_lt =
    fpp_b[Text,Text]("Text.<", "t1", "t2", Text.compare(_,_) < 0)

  val Text_gt =
    fpp_b[Text,Text]("Text.>", "t1", "t2", Text.compare(_,_) > 0)

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

  // don't remove the type annotation
  val Debug_crash: (Term.Name, Computation) =
    ("Debug.crash", (r => sys.error("called Debug.crash")): Computation.C0)

  val debugBuiltins = Map(Debug_crash)

  val builtins: Map[Name, Computation] =
    streamBuiltins ++ seqBuiltins ++ numericBuiltins ++
    booleanBuiltins ++ textBuiltins ++ debugBuiltins

  // Polymorphic one-argument function
  def fp_p[A,B](name: Name, arg: Name, f: A => B)
               (implicit A: Decode[A], B: Encode[B]): (Name, Computation) = {
    val body: Computation.C1P = (r,x0,x0b) => B.encode(r, f(A.decode(x0, x0b)))
    val decompile = Term.Id(name)
    name -> Return(new Lambda1(arg, body, decompile))
  }

  // Monomorphic one-argument function on unboxed values
  def _fu_u(name: Name,
            arg: Name,
            outputType: UnboxedType,
            f: LongUnaryOperator): (Name, Computation) = {
    val body: Computation.C1U = new Computation.C1U(outputType) {
      def raw(x0: U): U = f.applyAsLong(x0)
    }
    val decompile = Term.Id(name)
    val computation = Return(new Lambda1(arg, body, decompile))
    name -> computation
  }

//// a possibly faster model, but didn't see a clean analogue for fuu_u.
//  def fu_u(name: Name, arg: Name, outputType: UnboxedType)
//          (body: Computation.C1U): (Name, Computation) =
//    name -> Return(new Lambda1(arg, body, Some(outputType), Term.Id(name)))

  abstract class B_B { def test(b: Boolean): Boolean }
  def fb_b(name: Name, arg: Name, f: B_B): (Name, Computation) =
    _fu_u(name, arg, UnboxedType.Boolean,
         u => boolToUnboxed(f.test(unboxedToBool(u))))

  def fl_b(name: Name, arg: Name, f: LongPredicate): (Name, Computation) =
    _fu_u(name, arg, UnboxedType.Boolean,
         u => boolToUnboxed(f.test(unboxedToLong(u))))

  def fl_l(name: Name, arg: Name, f: LongUnaryOperator): (Name, Computation) =
    _fu_u(name, arg, UnboxedType.Int64,
          u => longToUnboxed(f.applyAsLong(unboxedToLong(u))))

  // UInt64 -> UInt64
  def fn_n(name: Name, arg: Name, f: LongUnaryOperator): (Name, Computation) =
    _fu_u(name, arg, UnboxedType.UInt64,
          u => longToUnboxed(f.applyAsLong(unboxedToLong(u))))

  def fp_z[A,B](name: Name, arg: Name, f: A => B)
               (implicit A: Decode[A], B: LazyEncode[B]): (Name, Computation) = {
    val body: Computation.C1P = (r,x0,x0b) => {
      B.encodeOp(r, f(A.decode(x0, x0b)), name, Value.fromParam(x0, x0b))
    }
    val lambda = new Lambda.Lambda1(arg, body, Term.Id(name))
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
    val lambda = new Lambda.ClosureForming(List(arg1, arg2), body, decompiled)
    name -> Return(lambda)
  }

  def fpp_z[A,B,C](name: Name, arg1: String, arg2: String, f: (A,B) => C)
                  (implicit A: Decode[A],
                   B: Decode[B],
                   C: LazyEncode[C]): (Name, Computation) = {
    val body: Computation.C2P =
      (r,x1,x0,x1b,x0b) => {
        val a = A.decode(x1, x1b)
        val b = B.decode(x0, x0b)
        C.encodeOp(r, f(a, b), name,
                   Value.fromParam(x1, x1b),
                   Value.fromParam(x0, x0b))
      }
    val decompiled = Term.Id(name)
    val lambda = new Lambda.ClosureForming(List(arg1, arg2), body, decompiled)
    name -> Return(lambda)
  }

  abstract class FLP_P[A,B] { def apply(l: Long, a: A): B }
  def flp_p[A:Decode,B:Encode](name: Name, arg1: Name, arg2: Name, f: FLP_P[A,B]) =
    _fup_p(name, arg1, arg2, (u, p: A) => f(unboxedToLong(u), p))

  abstract class FUP_P[A,B] { def apply(u: U, p: A): B }
  def _fup_p[A,B](name: Name, arg1: Name, arg2: Name, f: FUP_P[A,B])
                 (implicit A: Decode[A],
                          B: Encode[B]): (Name, Computation) = {
    val body: Computation.C2P = (r,x1,x0,_,x0b) =>
      B.encode(r, f(x1, A.decode(x0, x0b)))
    val decompiled = Term.Id(name)
    val lambda = new Lambda.ClosureForming(List(arg1, arg2), body, decompiled)
    name -> Return(lambda)
  }

  abstract class FPP_P[A,B,C] { def apply(a: A, b: B): C }
  def fpp_z[A,B,C](name: Name, arg1: Name, arg2: Name, f: FPP_P[A,B,C])
                  (implicit A: Decode[A], B: Decode[B], C: LazyEncode[C]): (Name, Computation) = {
    val body: Computation.C2P = (r,x1,x0,x1b,x0b) =>
      C.encodeOp(r, f(A.decode(x1,x1b), B.decode(x1,x1b)),
                 name,
                 Value.fromParam(x1,x1b),
                 Value.fromParam(x0,x0b))
    name -> Return(new Value.Lambda.ClosureForming(List(arg1, arg2), body, Term.Id(name)))
  }


  def flp_z[A:Decode,B:LazyEncode](name: Name, arg1: Name, arg2: Name, f: FLP_P[A,B]) =
    _fup_z[A,B](name, arg1, arg2, (u, a) => f(unboxedToLong(u), a))

  def _fup_z[A,B](name: Name, arg1: Name, arg2: Name, f: FUP_P[A,B])
                (implicit A: Decode[A],
                          B: LazyEncode[B]): (Name, Computation) = {
    val body: Computation.C2P = (r,x1,x0,x1b,x0b) =>
      B.encodeOp(r, f(x1, A.decode(x0, x0b)), name,
                 Value.fromParam(x1, x1b),
                 Value.fromParam(x0, x0b))

    val decompiled = Term.Id(name)
    val lambda = new Lambda.ClosureForming(List(arg1, arg2), body, decompiled)
    name -> Return(lambda)
  }

  abstract class FPP_B[A,B] { def apply(a: A, b: B): Boolean }
  def fpp_b[A:Decode,B:Decode](name: Name, arg1: Name, arg2: Name, f: FPP_B[A,B]) =
    _fpp_u[A,B](name, arg1, arg2, UnboxedType.Boolean, (a,b) => boolToUnboxed(f(a,b)))

  abstract class FPP_U[A,B] { def apply(a: A, b: B): U }
  def _fpp_u[A,B](name: Name, arg1: Name, arg2: Name,
                   outputType: UnboxedType, f: FPP_U[A,B])
                  (implicit A: Decode[A],
                            B: Decode[B]): (Name, Computation) = {
    val body: Computation =
      (r,_,_,_,x1,x0,_,x1b,x0b) => {
        r.boxed = outputType
        f(A.decode(x1, x1b), B.decode(x0, x0b))
      }
    val decompiled = Term.Id(name)
    val lambda = new Lambda.ClosureForming(List(arg1, arg2), body, decompiled)
    name -> Return(lambda)
  }

  def fll_l(name: Name, arg1: Name, arg2: Name, f: LongBinaryOperator): (Name, Computation) =
    _fuu_u(name, arg1, arg2, UnboxedType.Int64,
           (u1, u2) => longToUnboxed(f.applyAsLong(unboxedToLong(u1), unboxedToLong(u2))))

  def fdd_d(name: Name, arg1: Name, arg2: Name, f: DoubleBinaryOperator): (Name, Computation) =
    _fuu_u(name, arg1, arg2, UnboxedType.Float,
           (u1, u2) => doubleToUnboxed(f.applyAsDouble(unboxedToDouble(u1), unboxedToDouble(u2))))

  def fnn_n(name: Name, arg1: Name, arg2: Name, f: LongBinaryOperator) =
    _fuu_u(name, arg1, arg2, UnboxedType.UInt64,
           (u1, u2) => longToUnboxed(f.applyAsLong(unboxedToLong(u1), unboxedToLong(u2))))

  abstract class FLL_B { def apply(l1: Long, l2: Long): Boolean }
  def fll_b(name: Name, arg1: Name, arg2: Name, f: FLL_B): (Name, Computation) =
    _fuu_u(name, arg1, arg2, UnboxedType.Boolean,
           (u1, u2) => boolToUnboxed(f(unboxedToLong(u1), unboxedToLong(u2))))

  abstract class FDD_B { def apply(l1: Double, l2: Double): Boolean }
  def fdd_b(name: Name, arg1: Name, arg2: Name, f: FDD_B): (Name, Computation) =
    _fuu_u(name, arg1, arg2, UnboxedType.Boolean,
           (u1, u2) => boolToUnboxed(f(unboxedToDouble(u1), unboxedToDouble(u2))))

  def _fuu_u(name: Name,
             arg1: Name,
             arg2: Name,
             outputType: UnboxedType,
             f: LongBinaryOperator): (Name, Computation) = {
    val body = new Computation.C2U(outputType) {
      def raw(x1: U, x0: U): U = f.applyAsLong(x1, x0)
    }

    val decompiled = Term.Id(name)
    val lam = new Lambda(List(arg1, arg2), body, decompiled) {
      self =>

      override def saturatedNonTailCall(args: List[Computation]) = args match {
        case List(Return(Value.Unboxed(n1, _)),
                  Return(Value.Unboxed(n2, _))) =>
          val n3 = f.applyAsLong(n1,n2) // constant fold
          new Computation.C0U(outputType) {
            def raw: U = n3
          }
        case List(CompiledVar0,Return(Value.Unboxed(n, _))) =>
          new Computation.C1U(outputType) {
            def raw(x0: U): U = f.applyAsLong(x0, n)
          }
        case List(CompiledVar1,Return(Value.Unboxed(n, _))) =>
          new Computation.C2U(outputType) {
            def raw(x1: U, x0: U): U = f.applyAsLong(x1, n)
          }
        case List(Return(Value.Unboxed(n, _)), CompiledVar0) =>
          new Computation.C1U(outputType) {
            def raw(x0: U): U = f.applyAsLong(n,x0)
          }
        case List(Return(Value.Unboxed(n, _)), CompiledVar1) =>
          new Computation.C2U(outputType) {
            def raw(x1: U, x0: U): U = f.applyAsLong(n,x1)
          }
        case List(CompiledVar1,CompiledVar0) =>
          new Computation.C2U(outputType) {
            def raw(x1: U, x0: U): U = f.applyAsLong(x1,x0)
          }
        case List(CompiledVar0,CompiledVar1) =>
          new Computation.C2U(outputType) {
            def raw(x1: U, x0: U): U = f.applyAsLong(x0,x1)
          }
        case List(arg1: Computation.C2U, arg2: Computation.C2U) =>
          new Computation.C2U(outputType) {
            def raw(x1: U, x0: U): U = {
              val x1v = arg1.raw(x1, x0)
              val x0v = arg2.raw(x1, x0)
              f.applyAsLong(x1v, x0v)
            }
          }
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
              val body = new Computation.C1U(outputType) {
                def raw(x0: U): U = f.applyAsLong(n, x0)
              }
              new Lambda(self.names drop argCount, body,
                         Term.Apply(decompiled, term))
            case _ => sys.error("")
          }
          case _ => sys.error("can't underapply a function of 2 args with anything but 1 arg")
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
      (_, b) =>
        b.toValue.asInstanceOf[External].get.asInstanceOf[A]
  }

  /** Encode a scala type `A` in `Result => U` form. */
  trait Encode[-A] { def encode(r: Result, a: A): U }
  object Encode {
    implicit def encodeExternalDirect[A:Decompile]: Encode[A] =
      (r, a) => {
        r.boxed = External(a)
        U0
      }

    implicit val encodeValue: Encode[Value] =
      (r, a) => { r.boxed = a.toBoxed; a.toUnboxed }
    implicit val encodeUnsigned: Encode[Unsigned] =
      (r, a) => { r.boxed = UnboxedType.UInt64; longToUnboxed(a.raw) }
    implicit val encodeLong: Encode[Long] =
      (r, a) => { r.boxed = UnboxedType.Int64; longToUnboxed(a) }
    implicit val encodeInt: Encode[Int] =
      (r, a) => { r.boxed = UnboxedType.Int64; intToUnboxed(a) }
    implicit val encodeDouble: Encode[Double] =
      (r, a) => { r.boxed = UnboxedType.Float; doubleToUnboxed(a) }
  }

  trait LazyEncode[-A] {
    def encodeOp(r: Result, fnResult: A, fn: Name, args: Value*): U
  }
  object LazyEncode {
    implicit def encodeExternalLazy[A]: LazyEncode[A] =
      (r, a, fn, args) => {
        r.boxed = External(a, Term.Apply(Term.Id(fn), args.map(_.decompile): _*))
        U0
      }
  }

  trait Decompile[A] { def decompile(a: A): Term }

  object Decompile extends LowPriorityDecompile {
    implicit val decompileSequence: Decompile[Sequence[Value]] =
      s => Term.Sequence(s map (_.decompile))
    implicit val decompileText: Decompile[Text] =
      Term.Text(_)
    implicit val decompileUnit: Decompile[Unit] =
      u => BuiltinTypes.Unit.term
    implicit val decompileBoolean: Decompile[Boolean] =
      b => Term.Unboxed(boolToUnboxed(b), UnboxedType.Boolean)
    implicit def decompilePair[A,B](implicit A: Decompile[A], B: Decompile[B]): Decompile[(A,B)] =
      p => BuiltinTypes.Tuple.term(A.decompile(p._1), B.decompile(p._2))
  }

  trait LowPriorityDecompile {
    implicit val decompileValue: Decompile[Value] =
      v => v.decompile
  }

  abstract class External(val get: Any) extends Value
  object External {
    def apply[A](value: A, decompiled: Term): Value =
      new External(value) { def decompile = decompiled }
    def apply[A](value: A)(implicit A: Decompile[A]): Value =
      new External(value) {
        lazy val decompile: Term = A.decompile(value)
      }
  }
}


