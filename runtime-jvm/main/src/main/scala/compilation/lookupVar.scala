package org.unisonweb.compilation

import org.unisonweb.Term.{Name, Term}

object lookupVar extends ((Int, Name, Term) => Rt) {
  def apply(i: Int, name: Name, e: Term): Rt = i match {
    case 0 => new Arity1(e) {
      override def apply(rec: Rt, arg: D, argb: Rt, result: R) = {
        if (!(argb eq null)) result.boxed = argb
        arg
      }
      def bind(env: Map[Name,Rt]) = ()
    }
    case 1 => new Arity2(e) {
      override def apply(rec: Rt, x1: D, x2: Rt,
        arg: D, argb: Rt, result: R) = {
        if (!(argb eq null)) result.boxed = argb
        arg
      }
      def bind(env: Map[Name,Rt]) = ()
    }
    case 2 => new Arity3(e) {
      override def apply(rec: Rt, x1: D, x2: Rt, x3: D, x4: Rt,
        arg: D, argb: Rt, result: R) = {
        if (!(argb eq null)) result.boxed = argb
        arg
      }
      def bind(env: Map[Name,Rt]) = ()
    }
    case 3 => new Arity4(e) {
      override def apply(rec: Rt, x1: D, x2: Rt, x3: D, x4: Rt, x5: D, x6: Rt,
        arg: D, argb: Rt, result: R) = {
        if (!(argb eq null)) result.boxed = argb
        arg
      }
      def bind(env: Map[Name,Rt]) = ()
    }
    case i => new ArityN(i,e) {
      override def apply(rec: Rt, args: Array[Slot], result: R) = {
        val s = args(i)
        if (!(s.boxed eq null)) result.boxed = s.boxed
        s.unboxed
      }
      def bind(env: Map[Name,Rt]) = ()
    }
  }
}
