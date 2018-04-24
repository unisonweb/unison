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
  final def toValue = value
  override def isRef = true
}

abstract class Value extends Param {
  /** normalizes between refs and other values */
  final def toValue = this
  def decompile: Term

  /** Unboxed values will return an UnboxedType */
  @inline def toBoxed: Value = this
  /** true boxed values will return U0 */
  @inline def toUnboxed: U = U0
  def toResult(r: Result): U = { r.boxed = toBoxed; toUnboxed }
}

object Value {
  def apply(u: U, v: Value): Value =
    if (v.isType) Unboxed(u, v.asInstanceOf[UnboxedType]) else v

  def fromParam(u: U, b: Param): Value =
    if (b.isType) Unboxed(u, b.asInstanceOf[UnboxedType]) else b.toValue

  def apply(n: Long): Value = Value.Unboxed(longToUnboxed(n), UnboxedType.Integer)
  def apply(n: Double): Value = Value.Unboxed(doubleToUnboxed(n), UnboxedType.Float)
  def apply(b: Boolean): Value = Value.Unboxed(boolToUnboxed(b), UnboxedType.Boolean)

  case class Unboxed(n: U, typ: UnboxedType) extends Value {

    /** Unboxed values will return an UnboxedType */
    final override def toBoxed: Value = typ

    /** true boxed values will return U0 */
    final override def toUnboxed: U = n

    def decompile = Term.Unboxed(n, typ)
  }

  abstract class Lambda(
    final val arity: Int,
    final val body: Computation,
    final val unboxedType: Option[UnboxedType],
    val decompile: Term) extends Value { self =>

    def names: List[Name]
    def toComputation = Return(this)

    final def apply(r: R, top: StackPtr,
                    stackU: Array[U], x1: U, x0: U,
                    stackB: Array[B], x1b: B, x0b: B): U =
      body(r, this, top, stackU, x1, x0, stackB, x1b, x0b)

    def setSelfRec(rec: Lambda): Lambda = new Lambda(
      arity, body.setRec(rec), unboxedType, decompile
    ) {
      def names = self.names
    }

    def compose(f: Lambda): Lambda = {
      assert(arity == 1)
      val k: Computation = (r, rec, top, stackU, x1, x0, stackB, x1b, x0b) => {
        val v = evalLam(f,r,top,stackU,x1,x0,stackB,x1b,x0b)
        val vb = r.boxed
        self(r,top,stackU,U0,v,stackB,null,vb)
      }
      val compose = Term.Lam('f, 'g, 'x)('f.v('g.v('x))) // todo: intern this
      new Lambda(f.arity, k, self.unboxedType,
                 compose(self.decompile, f.decompile)) {
        val names = f.names
      }
    }

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
            case Return(v: Value.Lambda) =>
              v.setSelfRec(self.body.getRec getOrElse this)
            case c => sys.error(
              s"compiling a closed Term.Lambda failed to produce a Value.Lambda: $c")
          }
      }
  }
  object Lambda {
    final def toValue = this

    def apply(arity: Int, body: Computation, unboxedType: Option[UnboxedType],
              decompile: Term) =
      new Lambda(arity, body, unboxedType, decompile) {
        val names = decompile match { case Term.Lam(names, _) => names }
      }

    def unapply(l: Lambda): Option[(Int, Computation, Term)] =
      Some((l.arity, l.body, l.decompile))

    val identity: Lambda1 = {
      val c: Computation = (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
        r.boxed = x0b.toValue
        x0
      }
      Lambda1("x", c, None, Term.Lam('x)('x))
    }

    /** A `Lambda` of arity 1. */
    // todo: delete this and ClosureForming2 later
    case class Lambda1(arg1: Name, _body: Computation,
                       outputType: Option[UnboxedType], decompiled: Term)
      extends Lambda(1,_body,outputType,decompiled) {
      val names = List(arg1)
      override def underapply(builtins: Environment)(
        argCount: Arity, substs: Map[Name, Term]): Lambda =
        sys.error("a lambda with arity 1 cannot be underapplied")
    }

    /**
      * A `Lambda` of arity 2 that forms a closure when underapplied, rather
      * than specializing away the supplied argument.
      */
    // todo: delete this and Lambda1 later
    class ClosureForming2(arg1: Name, arg2: Name, body: Computation,
                          outputType: Option[UnboxedType], decompiled: Term)
      extends Lambda(2,body,outputType,decompiled) {
      val names = List(arg1,arg2)
      override def underapply(builtins: Environment)
                             (argCount: Arity, substs: Map[Name, Term])
      : Lambda = {
        assert(argCount == 1)
        val compiledArg = compileTop(builtins)(substs(arg1))
        val body1 = body maySetRec this
        val body2: Computation = (r,rec,top,stackU,_,x0,stackB,_,x0b) => {
          val compiledArgv = compiledArg(r, rec, top, stackU, U0, U0, stackB, null, null)
          body1(r, rec, top, stackU, compiledArgv, x0, stackB, r.boxed, x0b)
        }
        new Lambda1(arg1, body2, outputType, decompiled(substs(arg1)))
      }
    }
    abstract class ClosureForming(arity: Int, body: Computation,
                                  outputType: Option[UnboxedType],
                                  decompiled: Term, args: Array[B],
                                  selfRec: Option[Lambda] = None)
      extends Lambda(arity,body,outputType,decompiled) { self =>
      val namesArray = names.toArray
      override def underapply(builtins: compilation.Environment)(
        argCount: Int, substs: Map[Name, Term]): Value.Lambda = {
        assert(arity >= 1)
        val argsInOrder: Seq[Term] = (0 until argCount) map { i =>
          substs(namesArray(i + args.length))
        }
        val newArgs = argsInOrder map {
          case Term.Compiled(b) => b
          case Term.Self(_) => self
        }
        val body1 = body maySetRec this
        new ClosureForming(arity-argCount, body1, outputType,
                           decompiled(argsInOrder: _*), args ++ newArgs) {
          def names = self.names drop argCount
        }
      }
    }
  }

  case class Data(typeId: Id, constructorId: ConstructorId, fields: Array[Value])
    extends Value {
    def decompile: Term = Term.Constructor(typeId, constructorId)(fields.map(_.decompile): _*)
  }

  case class EffectPure(unboxed: U, boxed: Value) extends Value {
    def decompile = Term.EffectPure(Value(unboxed, boxed).decompile)
  }
  case class EffectBind(typeId: Id, constructorId: ConstructorId,
                        args: Array[Value], k: Lambda) extends Value {
    def decompile =
      Term.EffectBind(typeId, constructorId,
                      args.map(_.decompile).toList, k.decompile)
  }
}

sealed abstract class UnboxedType extends Value {
  def decompile = sys.error("Don't decompile a type.")
  override def toResult(r: R) = sys.error("A type is not a result.")
  override def isType = true
}

object UnboxedType {

  case object Integer extends UnboxedType
  case object Float extends UnboxedType
  case object Boolean extends UnboxedType
  case object Natural extends UnboxedType

}
