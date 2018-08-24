package org.unisonweb

object UnisonToScala {

  import compilation._
  import org.unisonweb.util.Unboxed

  type EnvTo[+A] = (Array[U], Array[B], StackPtr, Result) => A

  def toUnboxed1(p: (Term.Name, Computation)): EnvTo[Unboxed.F1[Param,Value]] =
    toUnboxed1(Builtins.lambdaFor(p))

  def unsafeToUnboxed1(f: Value): EnvTo[Unboxed.F1[Value,Value]] =
    toUnboxed1(f.asInstanceOf[Value.Lambda])

  def toUnboxed1(f: Value.Lambda): EnvTo[Unboxed.F1[Param,Value]] = {
    require (f.arity == 1)
    f.body match {
      case body: Computation.C1U =>
        (_, _, _, _) =>
          new Unboxed.F1[Param, Value] {
            def apply[x] = kvx => (u1,a,u2,x) => kvx(body.raw(u1), body.outputType, u2, x)
          }
      case _body =>
        (stackU, stackB, top, r) =>
          new Unboxed.F1[Param,Value] {
            def apply[x] = kvx => (u1,a,u2,x) => {
              val out = evalLam(f, r, top, stackU, U0, u1, stackB, null, a)
              kvx(out, r.boxed, u2, x)
            }
          }
    }
  }

  def unsafeToUnboxed2(f: Value): EnvTo[Unboxed.F2[Value,Value,Value]] =
    toUnboxed2(f.asInstanceOf[Value.Lambda])

  def toUnboxed2(p: (Term.Name, Computation)): EnvTo[Unboxed.F2[Value,Value,Value]] =
    toUnboxed2(Builtins.lambdaFor(p))

  def toUnboxed2(f: Value.Lambda): EnvTo[Unboxed.F2[Value,Value,Value]] =  {
    require(f.arity == 2)
    f.body match {
      case body: Computation.C2U =>
        (_, _, _, _) =>
          new Unboxed.F2[Param, Param, Value] {
            def apply[x] = kvx => (u1, a, u2, b, u3, x) => kvx(body.raw(u1, u2), body.outputType, u3, x)
          }

      case _body =>
        (stackU, stackB, top, r) =>
          new Unboxed.F2[Param, Param, Value] {
            def apply[x] = kvx => (u1, a, u2, b, u3, x) => {
              val out = evalLam(f, r, top, stackU, u1, u2, stackB, a, b)
              kvx(out, r.boxed, u3, x)
            }
          }
    }
  }
}
