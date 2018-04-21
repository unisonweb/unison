package org.unisonweb

object UnisonToScala {

  import compilation._
  import org.unisonweb.util.Unboxed

  type Env = (Array[U], Array[B], StackPtr, Result)

  def toUnboxed1(p: (Term.Name, Computation)): Env => Unboxed.F1[Param,Value] =
    toUnboxed1(Builtins.lambdaFor(p))

  def toUnboxed1(f: Value.Lambda): Env => Unboxed.F1[Param,Value] = {
    require (f.arity == 1)
    f.unboxedType.map[Env => Unboxed.F1[Param,Value]] {
      outputType =>
        env => {
          val (stackU, stackB, top, r) = env
          f.body match {
            case body: Computation.C1U => new Unboxed.F1[Param,Value] {
              def apply[x] = kvx => (u1,a,u2,x) => kvx(body(r,u1), outputType, u2, x)
            }
            case body => new Unboxed.F1[Param,Value] {
              def apply[x] = kvx => (u1,a,u2,x) => {
                val out = evalLam(f, r, top, stackU, U0, u1, stackB, null, a)
                kvx(out, r.boxed, u2, x)
              }
            }
          }
        }
    }.getOrElse(sys.error("`f` is expected to have an unboxed output type"))
  }

  def toUnboxed2(p: (Term.Name, Computation)): Env => Unboxed.F2[Value,Value,Value] =
    toUnboxed2(Builtins.lambdaFor(p))

  def toUnboxed2(f: Value.Lambda): Env => Unboxed.F2[Value,Value,Value] = {
    require(f.arity == 2)
    f.unboxedType.map[Env => Unboxed.F2[Value,Value,Value]] {
      outputType =>
        env => {
          val (stackU, stackB, top, r) = env
          f.body match {
            case body: Computation.C2U => new Unboxed.F2[Param, Param, Value] {
              def apply[x] = kvx => (u1, a, u2, b, u3, x) => kvx(body(r, u1, u2), outputType, u3, x)
            }
            case body => new Unboxed.F2[Param, Param, Value] {
              def apply[x] = kvx => (u1, a, u2, b, u3, x) => {
                val out = evalLam(f, r, top, stackU, u1, u2, stackB, a, b)
                kvx(out, r.boxed, u3, x)
              }
            }
          }
        }
    }.getOrElse(sys.error("`f` is expected to have an unboxed output type"))
  }
}
