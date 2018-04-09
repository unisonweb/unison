package org.unisonweb.util

import org.unisonweb.compilation.{U,U0}

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

    /**
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
  def choose[A](cond: F1[A,U], t: K[A], f: K[A]): K[A] = {
    val ccond = cond[A]((u,_,u2,a) => if (u != U0) t(u2,a) else f(u2,a))
    (u,a) => ccond(u,a,u,a)
  }

  /**
   * A continuation which acts as `segment1` until `cond` emits 0, then
   * acts as `segment2` forever thereafter.
   */
  def switchWhen0[A](cond: F1[A,U], segment1: K[A], segment2: K[A]): () => K[A] = () => {
    var switched = false
    val ccond = cond[A]((u,_,u2,a) => if (u == U0) { switched = true; segment1(u2,a) } else segment2(u2,a))
    (u,a) => ccond(u,a,u,a)
  }

  object F1 {
    /**
     * Convert a Scala `A => B` to an `F1[A,B]` that acts on boxed input and produces boxed output.
     * Named `B_B` since it takes one boxed input and produces boxed output.
     */
    def B_B[A,B](f: A => B): F1[A,B] = new F1[A,B] {
      def apply[x] = kbx => (u,a,u2,x) => kbx(U0, f(a), u2, x)
    }
  }

  object F2 {
    /**
     * Convert a Scala `(A,B) => C` to an `F2[A,B,C]` that acts on boxed input and produces boxed output.
     * Named `BB_B` since it takes two boxed input and produces boxed output.
     */
    def BB_B[A,B,C](f: (A,B) => C): F2[A,B,C] = new F2[A,B,C] {
      def apply[x] = kcx => (u,a,u2,b,u3,x) => kcx(U0, f(a,b), u3, x)
    }

    /**
     * An `F2[Unboxed[U],Unboxed[U],Unboxed[U]]` which works on unboxed input and produces unboxed output.
     * Named `UU_U` since it takes two unboxed input and produces unboxed output.
     */
    def UU_U(fn: UU_U): F2[Unboxed[U],Unboxed[U],Unboxed[U]] = new F2[Unboxed[U],Unboxed[U],Unboxed[U]] {
      def apply[x] = kux => (u,_,u2,_,u3,x) => kux(fn(u,u2),null,u3,x)
    }

    abstract class UU_U { def apply(u: U, u2: U): U }
  }
}
