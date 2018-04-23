package org.unisonweb
package util

import java.util.function._

/** Unboxed functions and continuations / callbacks. */
object Unboxed {

  /** A continuation receiving 1 value of type `A`, potentially unboxed. */
  abstract class K[-A] { self =>
    def apply(u: U, a: A): Unit
    final def toK2[B]: K2[A,B] = (u,a,u2,b) => self(u,a)
  }

  /** A continuation receiving an A and a B, both potentially unboxed. */
  abstract class K2[-A,-B] { def apply(u: U, a: A, u2: U, b: B): Unit }

  /** A continuation receiving an A, B, and C, all potentially unboxed. */
  abstract class K3[-A,-B,-C] { def apply(u: U, a: A, u2: U, b: B, u3: U, c: C): Unit }

  /**
   * Denotes functions `A -> B`. Unlike Scala's `A => B`, this function
   * can be passed unboxed input, and we can consume its output without
   * boxing.
   */
  abstract class F1[-A,+B] { self =>

    /*
     * Holy shit! A function from `A -> B` represented as a "continuation transformer".
     * The continuation which accepts a `B` value (potentially unboxed) is transformed
     * into a continuation which accepts an `A` value (potentially unboxed).
     *
     * The requirement that an `F1` be able to pass along an extra `X`, parametrically,
     * effectively adds products to the category.
     */
    def apply[X]: K2[B,X] => K2[A,X]

    /** Compose two `F1`s. */
    def map[C](f: F1[B,C]): F1[A,C] = new F1[A,C] {
      def apply[x] = kcx => self.apply(f.apply(kcx))
    }

    def andThen: K[B] => K[A] = kb => {
      val f = self.apply[AnyRef](kb.toK2)
      (u,a) => f(u,a,U0,null)
    }
  }

  /**
   * Denotes functions `(A,B) -> C`. Unlike Scala's `(A,B) => C`, this function
   * can be passed unboxed input, and we can consume its output without
   * boxing.
   */
  abstract class F2[-A,-B,+C] {

    def apply[X]: K2[C,X] => K3[A,B,X]

    final def andThen: K[C] => K2[A,B] =
      kc => { val kabx = apply(kc.toK2[AnyRef])
              (u,a,u2,b) => kabx(u,a,u2,b,U0,null) }
  }

  /**
   * Marker type with no instances. A `F[Unboxed[T]]` indicates that `F`
   * does not use the boxed portion of its representation and that there
   * exists a `U => T` for extracting a `T` from the unboxed portion of
   * its representation.
   */
  sealed abstract class Unboxed[T]

  object K {
    val noop: K[Any] = (_,_) => {}
  }

  /**
   * A continuation which invokes `t` whenver `cond` is nonzero on the
   * input, and which invokes `f` whenever `cond` is zero on the input.
   */
  def choose[A](cond: F1[A,Unboxed[Boolean]], t: K[A], f: K[A]): K[A] = {
    val ccond = cond[A]((u,b,u2,a) => if (unboxedToBool(u)) t(u2,a) else f(u2,a))
    (u,a) => ccond(u,a,u,a)
  }

  /**
   * A continuation which acts as `segment1` until `cond` emits 0, then
   * acts as `segment2` forever thereafter.
   */
  def switchWhen0[A](cond: F1[A,Unboxed[Boolean]], segment1: K[A], segment2: K[A]): () => K[A] = () => {
    var switched = false
    val ccond = cond[A]((u,_,u2,a) =>
                          if (switched || !unboxedToBool(u)) {
                            switched = true
                            segment2(u2,a)
                          } else segment1(u2,a))
    (u,a) => ccond(u,a,u,a)
  }

  object F1 {
    /** An F1 for boxed `C => D`. The B's in the name stand for "boxed". */
    def B_B[C,D](f: C => D): F1[C,D] = new F1[C,D] {
      def apply[x] =
        kbx => (_,c,ux,bx) => kbx(U0, f(c), ux, bx)
    }

    /** An F1 for unboxed `Long => Long` */
    def L_L(f: LongUnaryOperator) =
      new F1[Unboxed[Long],Unboxed[Long]] {
        def apply[X] =
          kux =>
            (u,_,ux,bx) =>
              kux(longToUnboxed(f.applyAsLong(unboxedToLong(u))), null,
                ux, bx)
      }

    def D_D(f: DoubleUnaryOperator) =
      new F1[Unboxed[Double],Unboxed[Double]] {
        def apply[X] =
          kux =>
            (u,_,ux,bx) =>
              kux(doubleToUnboxed(f.applyAsDouble(unboxedToDouble(u))), null,
                  ux, bx)
      }

    /** An F1 for unboxed `Long => Boolean` */
    def L_B(f: LongPredicate) =
      new F1[Unboxed[Long],Unboxed[Boolean]] {
        def apply[X]: K2[Unboxed[Boolean], X] => K2[Unboxed[U], X] =
          kux => (u,_,ux,bx) =>
            kux(boolToUnboxed(f.test(unboxedToLong(u))), null, ux, bx)
      }

    /** An F1 for unboxed `Double => Boolean` */
    def D_B(f: DoublePredicate) =
      new F1[Unboxed[Double],Unboxed[Boolean]] {
        def apply[X]: K2[Unboxed[Boolean], X] => K2[Unboxed[Double], X] =
          kux => (u,_,ux,bx) =>
            kux(boolToUnboxed(f.test(unboxedToDouble(u))), null, ux, bx)
      }

  }

  object F2 {
    /**
     * Convert a Scala `(A,B) => C` to an `F2[A,B,C]` that acts on boxed input and produces boxed output.
     * Named `BB_B` since it takes two boxed input and produces boxed output.
     */
    def BB_B[A,B,C](f: (A,B) => C): F2[A,B,C] = new F2[A,B,C] {
      def apply[x] = kcx =>
        (_,a,_,b,u3,x) => kcx(U0, f(a,b), u3, x)
    }

    /** An F2 for unboxed `(Long, Long) => Long` */
    def LL_L(fn: LongBinaryOperator): F2[Unboxed[Long],Unboxed[Long],Unboxed[Long]] =
      new F2[Unboxed[Long],Unboxed[Long],Unboxed[Long]] {
        def apply[x] =
          kux => (u,_,u2,_,u3,x) => kux(fn.applyAsLong(u,u2),null,u3,x)
      }

    /** An F2 for unboxed `(Long, Double) => Long` */
    abstract class LD_L { def apply(l: Long, d: Double): Long }
    def LD_L(fn: LD_L): F2[Unboxed[Long], Unboxed[Double], Unboxed[Long]] =
      new F2[Unboxed[Long], Unboxed[Double], Unboxed[Long]] {
        def apply[X]: K2[Unboxed[U], X] => K3[Unboxed[U], Unboxed[Double], X] =
          kux => (l,_,d,_,ux,bx) =>
            kux(longToUnboxed(fn(unboxedToLong(l), unboxedToDouble(d))), null, ux, bx)
      }

    /** An F2 for unboxed `(Double, Double) => Double` */
    def DD_D(fn: DoubleBinaryOperator) =
      new F2[Unboxed[Double],Unboxed[Double],Unboxed[Double]] {
        def apply[x] =
          kux => (u,_,u2,_,ux,bx) =>
            kux(
              doubleToUnboxed(
                fn.applyAsDouble(
                  unboxedToDouble(u),
                  unboxedToDouble(u2))),null,
              ux,bx)
      }
  }
}
