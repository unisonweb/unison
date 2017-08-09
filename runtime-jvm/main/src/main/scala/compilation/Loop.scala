package org.unisonweb.compilation

object loop extends ((TailCall, R) => D) {
  def apply(tc0: TailCall, r: R): D = {
    var tc = tc0
    while (!(tc eq null)) {
      val fn = tc.fn
      try {
        return ((fn.arity : @annotation.switch) match {
          case 1 => fn(fn, tc.x1, tc.x1b, r)
          case 2 => fn(fn, tc.x1, tc.x1b, tc.x2, tc.x2b, r)
          case 3 => fn(fn, tc.x1, tc.x1b, tc.x2, tc.x2b, tc.args(0).unboxed, tc.args(0).boxed, r)
          case 4 => fn(fn, tc.x1, tc.x1b, tc.x2, tc.x2b, tc.args(0).unboxed, tc.args(0).boxed, tc.args(1).unboxed, tc.args(1).boxed, r)
          case n => fn(fn, Array(Slot(tc.x1, tc.x1b), Slot(tc.x2, tc.x2b)) ++ tc.args, r)
        })
      }
      catch { case tc2: TailCall => tc = tc2 }
    }
    0.0
  }
}