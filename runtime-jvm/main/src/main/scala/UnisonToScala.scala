package org.unisonweb

object UnisonToScala {

  import org.unisonweb.util.Unboxed
  import compilation2._

  type Env = (Array[U], Array[B], StackPtr, Result)

  def toUnboxed1(f: Value.Lambda): Env => Unboxed.F1[Param,Value] = {
    require (f.arity == 1)
    env => {
      val (stackU, stackB, top, r) = env
      f.body match {
        case body: Computation.C1U => new Unboxed.F1[Param,Value] {
          def apply[x] = kvx => (u1,a,u2,x) => kvx(body(r,u1), null, u2, x)
        }
        case body => new Unboxed.F1[Param,Value] {
          def apply[x] = kvx => (u1,a,u2,x) => {
            val out = evalLam(f, r, top, stackU, U0, u1, stackB, null, a)
            kvx(out, r.boxed, u2, x)
          }
        }
      }
    }
  }

  def toUnboxed2(f: Value.Lambda): Env => Unboxed.F2[Param,Param,Value] = {
    require (f.arity == 2)
    env => {
      val (stackU, stackB, top, r) = env
      f.body match {
        case body : Computation.C2U => new Unboxed.F2[Param,Param,Value] {
          def apply[x] = kvx => (u1,a,u2,b,u3,x) => kvx(body(r,u1,u2), null, u3, x)
        }
        case body => new Unboxed.F2[Param,Param,Value] {
          def apply[x] = kvx => (u1,a,u2,b,u3,x) => {
            val out = evalLam(f, r, top, stackU, u1, u2, stackB, a, b)
            kvx(out, r.boxed, u3, x)
          }
        }
      }
    }
  }
}
