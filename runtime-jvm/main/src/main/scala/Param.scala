package org.unisonweb

import org.unisonweb.Term.{Name, Term}
import org.unisonweb.compilation._
import Term.Syntax._

sealed abstract class Param {
  def toValue: Value
  def isRef: Boolean = false
  def isType: Boolean = false
}

object Param {
  def apply(u: U, b: B): Param =
    if (b.isType) Value.Unboxed(u, b.asInstanceOf[UnboxedType])
    else b
}

final class Ref(val name: Name, var value: Value) extends Param {
  def toValue = value
  override def isRef = true
}

abstract class Value extends Param {
  final def toValue = this
  def decompile: Term
  def toResult(r: Result): U
}

object Value {
  def apply(u: U, b: Value): Value =
    if (b.isType) Unboxed(u, b.asInstanceOf[UnboxedType]) else b

  def fromParam(u: U, b: Param): Value =
    if (b.isType) Unboxed(u, b.asInstanceOf[UnboxedType]) else b.toValue

  case class Unboxed(n: U, typ: UnboxedType) extends Value {
    def decompile = Term.Unboxed(n, typ)
    def toResult(r: Result) =  {
      r.boxed = typ
      n
    }
  }

  abstract class Lambda(
    final val arity: Int,
    final val body: Computation,
    val decompile: Term) extends Value { self =>

    def names: List[Name]
    def toComputation = Return(this)

    final def apply(r: R, top: StackPtr,
                    stackU: Array[U], x1: U, x0: U,
                    stackB: Array[B], x1b: B, x0b: B): U =
      body(r, this, top, stackU, x1, x0, stackB, x1b, x0b)

    def compose(f: Lambda): Lambda = {
      assert(arity == 1)
      val k: Computation = (r, rec, top, stackU, x1, x0, stackB, x1b, x0b) => {
        val v = evalLam(f,r,top,stackU,x1,x0,stackB,x1b,x0b)
        val vb = r.boxed
        self(r,top,stackU,U0,v,stackB,null,vb)
      }
      val compose = Term.Lam('f, 'g, 'x)('f.v('g.v('x)))
      new Lambda(f.arity, k, compose(self.decompile, f.decompile)) {
        val names = f.names
      }
    }

    def toResult(r: Result) = { r.boxed = this; U0 }

    def saturatedNonTailCall(args: List[Computation]): Computation =
      compileStaticFullySaturatedNontailCall(this, args)

    def underapply(builtins: compilation.Environment)(
                   argCount: Int, substs: Map[Name, Term]): Value.Lambda =
      decompile match {
        case Term.Lam(names, body) =>
          compile(builtins)(
            Term.Lam(names drop argCount: _*)(ABT.substs(substs)(body)),
            Vector.empty, CurrentRec.none, RecursiveVars.empty, IsNotTail
          ) match {
            case Return(v: Value.Lambda) => v
            case c => sys.error(
              s"compiling a closed Term.Lambda failed to produce a Value.Lambda: $c")
          }
      }
  }
  object Lambda {
    def apply(arity: Int, body: Computation, decompile: Term) =
      new Lambda(arity, body, decompile) {
        val names = decompile match { case Term.Lam(names, _) => names }
      }

    def unapply(l: Lambda): Option[(Int, Computation, Term)] =
      Some((l.arity, l.body, l.decompile))

    /**
     * A `Lambda` of arity 2 that forms a closure when underapplied, rather
     * than specializing away the supplied argument.
     */
    class ClosureForming2(decompiled: Term, arg1: Name, arg2: Name, body: Computation)
      extends Lambda(2,body,decompiled) {
      val names = List(arg1,arg2)
      override def underapply(builtins: Environment)
                             (argCount: Arity, substs: Map[Name, Term])
                             : Lambda = {
        assert(argCount == 1)
        val compiledArg = compileTop(builtins)(substs(arg1))
        val body2: Computation = (r,rec,top,stackU,_,x0,stackB,_,x0b) => {
          val compiledArgv = compiledArg(r, rec, top, stackU, U0, U0, stackB, null, null)
          body(r, rec, top, stackU, compiledArgv, x0, stackB, r.boxed, x0b)
        }
        new Lambda(2 - argCount, body2, decompiled(names.take(argCount).map(substs): _*)) {
          def names: List[Name] = names.drop(argCount)

          override def underapply(builtins: Environment)(argCount: Arity, substs: Map[Name, Term]): Lambda =
            sys.error("a lambda with arity 1 cannot be underapplied")
        }
      }
    }
    abstract class ClosureForming(arity: Int, body: Computation, decompiled: Term,
                                  args: Array[B]) extends Lambda(arity,body,decompiled) { self =>
      val namesArray = names.toArray
      override def underapply(builtins: compilation.Environment)(
                              argCount: Int, substs: Map[Name, Term]): Value.Lambda = {
        val argsInOrder: Seq[Term] = (0 until argCount) map { i =>
          substs(namesArray(i + args.length))
        }
        val newArgs = argsInOrder map {
          case Term.Compiled(b) => b
          case Term.Self(_) => self
        }
        new ClosureForming(arity-argCount, body, decompiled(argsInOrder: _*), args ++ newArgs) {
          def names = self.names drop argCount
        }
      }
    }
  }

  case class Data(typeId: Id, constructorId: ConstructorId, fields: Array[Value])
    extends Value {
    def decompile: Term = Term.Constructor(typeId, constructorId)(fields.map(_.decompile): _*)
    def toResult(r: R): U = { r.boxed = this; U0 }
  }

}

sealed abstract class UnboxedType extends Value {
  def decompile = sys.error("Don't decompile a type.")
  def toResult(r: R) = sys.error("A type is not a result.")
  override def isType = true
}

object UnboxedType {

  case object Integer extends UnboxedType
  case object Float extends UnboxedType
  case object Boolean extends UnboxedType
  case object Natural extends UnboxedType

}
