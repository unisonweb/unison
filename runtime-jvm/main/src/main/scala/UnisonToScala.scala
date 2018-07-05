package org.unisonweb

object UnisonToScala {

  import compilation._
  import org.unisonweb.util.Unboxed

  type Env = (Array[U], Array[B], StackPtr, Result)

  def toUnboxed1(p: (Term.Name, Computation)): Env => Unboxed.F1[Param,Value] =
    toUnboxed1(Builtins.lambdaFor(p))

  def unsafeToUnboxed1(f: Value): Env => Unboxed.F1[Param,Value] =
    toUnboxed1(f.asInstanceOf[Value.Lambda])

  def toUnboxed1(f: Value.Lambda): Env => Unboxed.F1[Param,Value] = {
    require (f.arity == 1)
    f.body match {
      case body: Computation.C1U =>
        _env =>
          new Unboxed.F1[Param, Value] {
            def apply[x] = kvx => (u1,a,u2,x) => kvx(body.raw(u1), body.outputType, u2, x)
          }
      case _body =>
        env =>
          val (stackU, stackB, top, r) = env
          new Unboxed.F1[Param,Value] {
            def apply[x] = kvx => (u1,a,u2,x) => {
              val out = evalLam(f, r, top, stackU, U0, u1, stackB, null, a)
              kvx(out, r.boxed, u2, x)
            }
          }
    }
  }

  def unsafeToUnboxed2(f: Value): Env => Unboxed.F2[Value,Value,Value] =
    toUnboxed2(f.asInstanceOf[Value.Lambda])

  def toUnboxed2(p: (Term.Name, Computation)): Env => Unboxed.F2[Value,Value,Value] =
    toUnboxed2(Builtins.lambdaFor(p))

  def toUnboxed2(f: Value.Lambda): Env => Unboxed.F2[Value,Value,Value] = {
    require(f.arity == 2)
    f.body match {
      case body: Computation.C2U =>
        _env =>
          new Unboxed.F2[Param, Param, Value] {
            def apply[x] = kvx => (u1, a, u2, b, u3, x) => kvx(body.raw(u1, u2), body.outputType, u3, x)
          }

      case _body =>
        env =>
          val (stackU, stackB, top, r) = env
          new Unboxed.F2[Param, Param, Value] {
            def apply[x] = kvx => (u1, a, u2, b, u3, x) => {
              val out = evalLam(f, r, top, stackU, u1, u2, stackB, a, b)
              kvx(out, r.boxed, u3, x)
            }
          }
    }
  }
}
