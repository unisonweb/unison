package org.unisonweb.util

import org.unisonweb.compilation2.{U,U0}

object Unboxed {

  abstract class K[-A] { self =>
    def apply(u: U, a: A): Unit
    final def toK2[B]: K2[A,B] = (u,a,u2,b) => self(u,a)
  }
  abstract class K2[-A,-B] { def apply(u: U, a: A, u2: U, b: B): Unit }
  abstract class K3[-A,-B,-C] { def apply(u: U, a: A, u2: U, b: B, u3: U, c: C): Unit }

  abstract class F1[A,B] { self =>
    def apply[X]: K2[B,X] => K2[A,X]
    def contramap: K[B] => K[A] = kb => {
      val f = self.apply[AnyRef](kb.toK2)
      (u,a) => f(u,a,U0,null)
    }
  }

  abstract class F2[A,B,C] {
    def apply[X]: K2[C,X] => K3[A,B,X]
  }

  object K {
    val noop: K[Any] = (_,_) => {}
  }

  def choose[A](cond: F1[A,U], t: K[A], f: K[A]): K[A] = {
    val ccond = cond[A]((u,_,u2,a) => if (u != U0) t(u2,a) else f(u2,a))
    (u,a) => ccond(u,a,u,a)
  }

  def switchWhen[A](cond: F1[A,U], segment1: K[A], segment2: K[A]): () => K[A] = () => {
    var switched = false
    val ccond = cond[A]((u,_,u2,a) => if (u != U0) { switched = true; segment1(u2,a) } else segment2(u2,a))
    (u,a) => ccond(u,a,u,a)
  }

  object F1 {
    def boxedScalaFunction[A,B](f: A => B): F1[A,B] = new F1[A,B] {
      def apply[x] = kbx => (u,a,u2,x) => kbx(U0, f(a), u2, x)
    }
  }

  object F2 {
    def boxedScalaFunction[A,B,C](f: (A,B) => C): F2[A,B,C] = new F2[A,B,C] {
      def apply[x] = kcx => (u,a,u2,b,u3,x) => kcx(U0, f(a,b), u3, x)
    }
  }
}
