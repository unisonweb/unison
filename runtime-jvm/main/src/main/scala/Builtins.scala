package org.unisonweb

import compilation._
import Term.{Apply, Name, Term}
import org.unisonweb.compilation.Value.Lambda
import org.unisonweb.util.Sequence
import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
import java.util.function.{LongBinaryOperator, LongUnaryOperator}

/* Sketch of convenience functions for constructing builtin functions. */
object Builtins {

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
    fpp_p("Sequence.take", "n", "seq",
         (n: U, seq: Sequence[Value]) => seq take n.toLong)

  val Sequence_size =
    fp_p("Sequence.size", "seq", (seq: Sequence[Value]) => seq.size)

  def c0[A:Decompile](name: String, a: => A)
                     (implicit A: Encode[A]): (Name, Computation) =
    (name, (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => A.encode(r, a))

  def termFor(b: (Name, Computation)): Term = Term.Builtin(b._1)

  @inline def boolToNum(b: Boolean): U = if (b) True else False

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

  val numericBuiltins = Map(
    fuu_u("+", "x", "y", _ + _),
    fuu_u("*", "x", "y", _ * _),
    fuu_u("-", "x", "y", _ - _),
    fuu_u("/", "x", "y", _ / _),
    fuu_u("==", "x", "y", (x, y) => boolToNum(x == y)),
    fuu_u("!=", "x", "y", (x, y) => boolToNum(x != y)),
    fuu_u("<=", "x", "y", (x, y) => boolToNum(x <= y)),
    fuu_u(">=", "x", "y", (x, y) => boolToNum(x >= y)),
    fuu_u("<", "x", "y", (x, y) => boolToNum(x < y)),
    fuu_u(">", "x", "y", (x, y) => boolToNum(x > y)),
    fu_u("signum", "x", _.signum),
    fu_u("negate", "x", -_)
  )

  val booleanBuiltins = Map(
    fu_u("not", "b", b => boolToNum(b == U0)),
  )

  val builtins = seqBuiltins ++ numericBuiltins ++ booleanBuiltins

  // Polymorphic one-argument function
  def fp_p[A,B](name: Name, arg: Name, f: A => B)
               (implicit A: Decode[A], B: Encode[B]): (Name, Computation) = {
    val body: Computation =
      (r,_,_,_,_,x0,_,_,x0b) => B.encode(r, f(A.decode(x0, x0b)))
    val decompiled = Term.Builtin(name)
    val lambda = new Lambda(1, body, decompiled) {
      def names: List[Name] = List(arg)
      override def underapply(builtins: Name => Computation)(
                              argCount: Arity, substs: Map[Name, Term]): Lambda =
        sys.error("a lambda with arity 1 cannot be underapplied")
    }
    name -> Return(lambda, decompiled)
  }

  // Monomorphic one-argument function on unboxed values
  def fu_u(name: Name, arg: Name, f: LongUnaryOperator): (Name, Computation) = {
    val body: Computation.C1U = (r,x0) => {
      r.boxed = null
      f.applyAsLong(x0)
    }
    val ns = List(arg)
    val decompile = Term.Builtin(name)
    name -> Return(new Lambda(1, body, decompile) { def names = ns }, decompile)
  }

  def fpp_p[A,B,C](name: String, arg1: String, arg2: String, f: (A,B) => C)
                  (implicit A: Decode[A],
                            B: Decode[B],
                            C: Encode[C]): (Name, Computation) = {
    val body: Computation =
      (r,_,_,_,x1,x0,_,x1b,x0b) =>
        C.encode(r, f(A.decode(x1, x1b), B.decode(x0, x0b)))
    val decompiled = Term.Builtin(name)
    val lambda = new Lambda(2, body, decompiled) { outer =>
      def names: List[Name] = List(arg1, arg2)

      // todo: generalize this into 1..K implementations
      override def underapply(builtins: Name => Computation)(argCount: Arity, substs: Map[Name, Term]): Lambda = {
        assert(argCount == 1)
        val compiledArg = compileTop(builtins)(substs(arg2))
        val body2: Computation = (r,rec,top,stackU,_,x0,stackB,_,x0b) => {
          val compiledArgv = eval(compiledArg, r, rec, top, stackU, U0, U0, stackB, null, null)
          body(r, rec, top, stackU, compiledArgv, x0, stackB, r.boxed, x0b)
        }
        new Lambda(outer.arity - argCount, body2, decompiled(names.map(substs): _*)) {
          def names: List[Name] = names.drop(argCount)

          override def underapply(builtins: Name => Computation)(argCount: Arity, substs: Map[Name, Term]): Lambda =
            sys.error("a lambda with arity 1 cannot be underapplied")
        }
      }

    }
    name -> Return(lambda, decompiled)
  }

  def fuu_u(name: Name,
            n1: Name,
            n2: Name,
            f: LongBinaryOperator): (Name, Computation) = {
    val body: Computation.C2U = (r,x1,x0) => {
      r.boxed = null
      f.applyAsLong(x1, x0)
    }

    val decompiled = Term.Builtin(name)

    val lam = new Lambda(2, body, decompiled) { self =>
      def names = List(n1, n2)
      override def saturatedNonTailCall(args: List[Computation]) = args match {
        case List(Return(Value.Num(n1)), Return(Value.Num(n2))) =>
          val n3 = f.applyAsLong(n1,n2) // constant fold
          val c : Computation.C0U = r => { r.boxed = null; n3 }
          c
        case List(CompiledVar0,Return(Value.Num(n))) =>
          val c : Computation.C1U = (r,x0) => {
            r.boxed = null
            f.applyAsLong(x0, n)
          }
          c
        case List(CompiledVar1,Return(Value.Num(n))) =>
          val c : Computation.C2U = (r,x1,_) => {
            r.boxed = null
            f.applyAsLong(x1, n)
          }
          c
        case List(Return(Value.Num(n)), CompiledVar0) =>
          val c: Computation.C1U = (r,x0) => {
            r.boxed = null
            f.applyAsLong(n,x0)
          }
          c
        case List(Return(Value.Num(n)), CompiledVar1) =>
          val c: Computation.C2U = (r,x1,_) => {
            r.boxed = null
            f.applyAsLong(n,x1)
          }
          c
        case List(CompiledVar1,CompiledVar0) =>
          val c: Computation.C2U = (r,x1,x0) => {
            r.boxed = null
            f.applyAsLong(x1,x0)
          }
          c
        case List(CompiledVar0,CompiledVar1) =>
          val c: Computation.C2U = (r,x1,x0) => {
            r.boxed = null
            f.applyAsLong(x0,x1)
          }
          c
        case List(arg1: Computation.C2U, arg2: Computation.C2U) =>
          val c: Computation.C2U = (r,x1,x0) =>
            // no need to null out r.boxed, as that will be done by arg1 and arg2
            f.applyAsLong(arg1(r,x1,x0), arg2(r,x1,x0))
          c
        case List(arg1,arg2) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          val x1v = eval(arg1,r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
          val x0v = eval(arg2,r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
          r.boxed = null
          f.applyAsLong(x1v, x0v)
        }
      }
      override def underapply(builtins: Name => Computation)
                             (argCount: Int, substs: Map[Name, Term]): Lambda =
        substs.toList match {
          case List((_,term)) => term match {
            case Term.Compiled(p: Param) =>
              val n = p.toValue.asInstanceOf[Value.Num].n
              val body: Computation =
                (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
                  r.boxed = null
                  f.applyAsLong(n, x0)
                }
              new Lambda(1, body, Term.Apply(decompiled, term)) {
                def names = self.names drop argCount
              }
            case _ => sys.error("")
          }
          case _ => sys.error("unpossible")
        }
    }
    name -> Return(lam, decompiled)
  }

  trait Decode[+T] { def decode(u: U, b: B): T }
  object Decode extends Decode0 {
    implicit val decodeValue: Decode[Value] =
      (u, b) => Value.fromParam(u, b)

    implicit val decodeU: Decode[U] = (u,_) => u
    implicit val decodeDouble: Decode[Double] = (u,_) => longBitsToDouble(u)

// TODO: If we include this implicit, it gets selected, even if the function
// is just asking for a `Decode[Value]`.
// See https://issues.scala-lang.org/browse/SI-2509
//
//    implicit val decodeLambda: Decode[Lambda] =
//      (_, b) => b.toValue.asInstanceOf[Lambda]
  }
  trait Decode0 {
    implicit def decodeAssumeExternal[A]: Decode[A] =
      (_, b) => b.toValue.asInstanceOf[External].get.asInstanceOf[A]
  }

  trait Encode[-A] { def encode(r: Result, a: A): U }
  object Encode {
    implicit def encodeExternal[A:Decompile]: Encode[A] =
      (r, a) => { r.boxed = External(a); U0 }
    implicit val encodeLong: Encode[Long] =
      (r, a) => { r.boxed = null; a }
    implicit val encodeDouble: Encode[Double] =
      (r, a) => { r.boxed = null; doubleToRawLongBits(a) }
  }

  trait Decompile[A] { def decompile(a: A): Term }
  object Decompile {
    implicit val decompileSequence: Decompile[Sequence[Value]] =
      // todo decompile to sequence literals once available
      s => s.foldLeft(termFor(Sequence_empty)) {
        (term, v) => Apply(termFor(Sequence_snoc), term, v.decompile)
      }
  }

  abstract class External(val get: Any) extends Value {
    def toResult(r: R) = { r.boxed = this; U0 }
  }
  object External {
    def apply[A](value: A)(implicit A: Decompile[A]) =
      new External(value) {
        def decompile: Term = A.decompile(value)
      }
  }

}


