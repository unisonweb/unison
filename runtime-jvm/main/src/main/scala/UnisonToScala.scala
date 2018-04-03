package org.unisonweb

object UnisonToScala {

  import org.unisonweb.util.Unboxed
  import compilation2._

  type Env = (Array[U], Array[B], StackPtr, Result)

  def toUnboxed1(f: Value.Lambda): Unboxed.F1[Env,Param,Value] = {
    require (f.arity == 1)
    env => set => {
      val (stackU, stackB, top, r) = env
      f.body match {
        case body: Computation.C1U => (u1,a) => body(r,u1)
        case body => (u,a) => {
          val out = evalLam(f, r, top, stackU, U0, u, stackB, null, a)
          set(r.boxed)
          out
        }
      }
    }
  }

  def toUnboxed2(f: Value.Lambda): Unboxed.F2[Env,Param,Param,Value] = {
    require (f.arity == 2)
    env => set => {
      val (stackU, stackB, top, r) = env
      f.body match {
        case body : Computation.C2U => (u1,a,u2,b) => body(r,u1,u2)
        case body => (u1,a,u2,b) => {
          val out = evalLam(f, r, top, stackU, u1, u2, stackB, a, b)
          set(r.boxed)
          out
        }
      }
    }
  }
}
