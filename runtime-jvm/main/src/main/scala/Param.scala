package org.unisonweb

import org.unisonweb.Term.{Name, Term}
import org.unisonweb.compilation._

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
//      r.boxed = typ // todo: can we elide this?
      n
    }
  }

  abstract class Lambda(
    final val arity: Int,
    final val body: Computation,
    val decompile: Term) extends Value {

    def names: List[Name]
    def toComputation = Return(this)

    final def apply(r: R, top: StackPtr,
                    stackU: Array[U], x1: U, x0: U,
                    stackB: Array[B], x1b: B, x0b: B): U =
      body(r, this, top, stackU, x1, x0, stackB, x1b, x0b)

    def toResult(r: Result) = { r.boxed = this; U0 }

    def saturatedNonTailCall(args: List[Computation]): Computation =
      compileStaticFullySaturatedNontailCall(this, args)

    def underapply(builtins: Name => Computation)(
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
  }

  case class Data(typeId: Hash, constructorId: ConstructorId, fields: Array[Value])
    extends Value {
    def decompile: Term = Term.Hashref(Hash.constructorId(typeId, constructorId))
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
