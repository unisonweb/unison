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

  def apply(n: Long): Value = Value.Unboxed(longToUnboxed(n), UnboxedType.Int64)
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
    // the lambda decompiled form may have one free var, referring to itself
    decompileWithPossibleFreeVar: Term) extends Value { self =>

    assert (Term.freeVars(decompileWithPossibleFreeVar).size <= 1)

    def decompile = Term.freeVars(decompileWithPossibleFreeVar).toList match {
      case Nil => decompileWithPossibleFreeVar
      case List(name) =>
        ABT.subst(name, Term.Compiled(this, name))(decompileWithPossibleFreeVar)
    }

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
            case Return(v: Value.Lambda) => v
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
        val body2: Computation = (r,rec,top,stackU,_,x0,stackB,_,x0b) => {
          val compiledArgv = compiledArg(r, rec, top, stackU, U0, U0, stackB, null, null)
          body(r, rec, top, stackU, compiledArgv, x0, stackB, r.boxed, x0b)
        }
        new Lambda1(arg1, body2, outputType, decompiled(substs(arg1)))
      }
    }

    abstract class ClosureForming(arity: Int, body: Computation,
                                  outputType: Option[UnboxedType],
                                  decompiled: Term)
        extends Lambda(arity,body,outputType,decompiled) { self =>
      val namesArray = names.toArray

      /** Underapply this `Lambda`, passing 1 argument (named `substName`). */
      final def underapply1(substName: Name, substTerm: Term): ClosureForming = {
        assert(arity >= 1)
        val arg = substTerm match { case Term.Compiled(b,_) => b }
        val v = arg.toValue
        val argv = v.toUnboxed
        val argvb = v.toBoxed
        // conceptually, `body2` has to insert `arg` BEFORE the new args
        // on the stack, and then call `body`
        val body2: Computation = arity match {
          // stack passed to `body2`: [a]
          // stack passed to `body` : [arg,a]
          case 2 => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) =>
            body(r,rec,top,stackU,argv,x0,stackB,argvb,x0b)
          // stack passed to `body2`: [a,b]
          // stack passed to `body` : [arg,a,b]
          case 3 => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
            top.push1(stackU,stackB,argv,argvb)
            body(r,rec,top.inc,stackU,x1,x0,stackB,x1b,x0b)
          }
          // stack passed to `body2`: [...,y,z]
          // stack passed to `body` : [arg,...,y,z]
          case n => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
            // need to insert arg1 before all the (n - 1 - K) args that
            // are already on the stack, and shift all those args over
            ???
          }
        }
        new ClosureForming(arity-1, body2, outputType, decompiled(substTerm)) {
          def names = self.names drop 1
        }
      }

      // todo: try for more efficient implementation of underapply, O(n) vs n^2
      // esp when there are multiple stages of underapply for functions with
      // large arities

      override def underapply(builtins: compilation.Environment)(
        argCount: Int, substs: Map[Name, Term]): Value.Lambda = {
        if (argCount == 1) underapply1(substs.head._1, substs.head._2)
        else {
          (0 until argCount).foldLeft(this) { (lam,i) =>
            val name = namesArray(i)
            lam.underapply1(name, substs(name))
          }
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

  case object Int64 extends UnboxedType
  case object UInt64 extends UnboxedType
  case object Float extends UnboxedType
  case object Boolean extends UnboxedType

}
