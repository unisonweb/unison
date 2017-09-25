package org.unisonweb.benchmark

import org.unisonweb.Term
import org.unisonweb.Term.{Name, Term}
import org.unisonweb.benchmark.Fib.decompileSlot
import org.unisonweb.compilation._

object Builtins {
  implicit class Arithmetic(a: Term) {
    def -(b: Term) = Term.Builtin("-")(a,b)
    def +(b: Term) = Term.Builtin("+")(a,b)
    def *(b: Term) = Term.Builtin("*")(a,b)
    def <(b: Term) = Term.Builtin("<")(a,b)
    def >(b: Term) = Term.Builtin(">")(a,b)
  }

  // todo: move these somewhere
  // note: App initializes body vals too lazily
  def builtins : Name => Computation = ({
    case s@"-" => mkBuiltin(s, _ - _)
    case s@"+" => mkBuiltin(s, _ + _)
    case s@"*" => mkBuiltin(s, _ * _)
    case s@"<" => mkBuiltin(s, (l, r) => if (l < r) 1.0 else 0.0)
    case s@">" => mkBuiltin(s, (l, r) => if (l > r) 1.0 else 0.0)
    case s => sys.error("unknown builtin: " + s)
  }: String => Computation).compose[Name](_.toString)

  def mkBuiltin(name: Name, f: (Double, Double) => Double) = {
    val term = Term.Builtin(name)
    Return {
      new Lambda {
        def arity = 2
        def apply(rec: Lambda, r: R) = { r.boxed = this; 0.0 }

        def apply(rec: Lambda, x0: D, x0b: V, r: R) = { r.boxed = new Lambda {
          def arity = 1

          def apply(rec: Lambda, r: R) = { r.boxed = this; 0.0 }

          def apply(rec: Lambda, x1: D, x1b: V, r: R) = { r.boxed = null; f(x1, x0) }

          def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, r: R) = ???
          def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, r: R) = ???
          def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, x3: D, x3b: V, r: R) = ???
          def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, x3: D, x3b: V, x4: D, x4b: V, r: R): D = ???
          def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, x3: D, x3b: V, x4: D, x4b: V, x5: D, x5b: V, r: R): D = ???
          def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, x3: D, x3b: V, x4: D, x4b: V, x5: D, x5b: V, x6: D, x6b: V, r: R): D = ???
          def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, x3: D, x3b: V, x4: D, x4b: V, x5: D, x5b: V, x6: D, x6b: V, x7: D, x7b: V, r: R): D = ???
          def apply(rec: Lambda, xs: Array[Slot], r: R) = ???
          def decompile = term(decompileSlot(x0, x0b))
        }; 0.0 }

        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, r: R) = { r.boxed = null; f(x1, x0) }

        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, r: R) = ???
        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, x3: D, x3b: V, r: R) = ???
        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, x3: D, x3b: V, x4: D, x4b: V, r: R): D = ???
        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, x3: D, x3b: V, x4: D, x4b: V, x5: D, x5b: V, r: R): D = ???
        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, x3: D, x3b: V, x4: D, x4b: V, x5: D, x5b: V, x6: D, x6b: V, r: R): D = ???
        def apply(rec: Lambda, x0: D, x0b: V, x1: D, x1b: V, x2: D, x2b: V, x3: D, x3b: V, x4: D, x4b: V, x5: D, x5b: V, x6: D, x6b: V, x7: D, x7b: V, r: R): D = ???
        def apply(rec: Lambda, xs: Array[Slot], r: R) = ???
        def decompile = term
      }
    }(term)
  }
}
