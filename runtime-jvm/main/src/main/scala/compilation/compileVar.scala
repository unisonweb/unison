package org.unisonweb.compilation

import org.unisonweb.Term.Name

object compileVar extends ((Name, TermC, Boolean) => Rt) {
  def apply(name: Name, e: TermC, compileAsFree: Boolean): Rt =
    if (compileAsFree) new Rt {
      var rt: Rt = null
      def arity = rt.arity
      def apply(rec: Rt, r: R) = rt(rec,r)
      def apply(rec: Rt, x1: D, x1b: Rt, r: R) = rt(rec,x1,x1b,r)
      def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = rt(rec,x1,x1b,x2,x2b,r)
      def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = rt(rec,x1,x1b,x2,x2b,x3,x3b,r)
      def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = rt(rec,x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
      def apply(rec: Rt, args: Array[Slot], r: R) = rt(rec,args,r)
      // if (rt eq null) throw new InfiniteLoopError(name)
      // todo : possibly try / catch NPEs
      override def bind(env: Map[Name,Rt]) = env.get(name) match {
        case Some(rt2) => rt = rt2
        case _ => () // not an error, just means that some other scope will bind this free var
      }
      override def isEvaluated = !(rt eq null)
      // let rec loop = loop; loop
      // let rec ping = pong; pong = ping; ping
      // let rec ping x = pong (x + 1); pong x = ping (x + 1); ping
      def decompile = if (rt eq null) unTermC(e) else rt.decompile
    }
    else env(e).indexOf(name) match {
      case -1 => sys.error("unknown variable: " + name)
      case i => lookupVar(i, name, unTermC(e))
    }
}