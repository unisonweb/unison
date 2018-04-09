package org.unisonweb

import java.util.function.{LongUnaryOperator, LongBinaryOperator}
import org.unisonweb.compilation2._
import org.unisonweb.compilation2.Value.Lambda
import org.unisonweb.Term.{Name, Term}

object Lib2 {

  val builtins: Name => Computation =
    List[(Name, LongBinaryOperator)](
      ("+", _ + _),
      ("-", _ - _),
      ("*", _ * _),
      ("/", _ / _),
      ("==", (n1, n2) => boolToNum(n1 == n2)),
      ("!=", (n1, n2) => boolToNum(n1 != n2)),
      ("<=", (n1, n2) => boolToNum(n1 <= n2)),
      (">=", (n1, n2) => boolToNum(n1 >= n2)),
      ("<", (n1, n2) => boolToNum(n1 < n2)),
      (">", (n1, n2) => boolToNum(n1 > n2)),
    ).map {
      case (name, f) =>
        val term = Term.Builtin(name)
        name -> Return(fuu_u(term, List("x1", "x2"), f), term)
    }.toMap ++ Map[Name, LongUnaryOperator](
      ("not", b => if (b == False) True else False),
      ("negate", x => -x)
    ).map {
      case (name, f) =>
        val term = Term.Builtin(name)
        name -> Return(fu_u(term, "x", f), term)
    }.toMap ++
    Builtins.builtins

  @inline def boolToNum(b: Boolean): U = if (b) True else False

  def fu_u(decompiled: Term, n: Name, f: LongUnaryOperator): Lambda = {
    val body: Computation.C1U = (r,x0) => {
      r.boxed = null
      f.applyAsLong(x0)
    }
    val ns = List(n)
    new Lambda(1, body, decompiled) { def names = ns }
  }

  def fuu_u(decompiled: Term, ns: List[Name], f: LongBinaryOperator): Lambda = {
    val body: Computation.C2U = (r,x1,x0) => {
      r.boxed = null
      f.applyAsLong(x1, x0)
    }

    new Lambda(2, body, decompiled) { self =>
      def names = ns
      override def saturatedNonTailCall(args: List[Computation]) = args match {
        case List(Return(Value.Num(n1)), Return(Value.Num(n2))) =>
          val n3 = f.applyAsLong(n1,n2) // constant fold
          val c : Computation.C0U = r => { r.boxed = null; n3 }
          c
        case List(CompiledVar0,Return(Value.Num(n))) =>
          val c : Computation.C1U = (r,x0) => {
            r.boxed = null
            f.applyAsLong(x0, n)
          }
          c
        case List(CompiledVar1,Return(Value.Num(n))) =>
          val c : Computation.C2U = (r,x1,_) => {
            r.boxed = null
            f.applyAsLong(x1, n)
          }
          c
        case List(Return(Value.Num(n)), CompiledVar0) =>
          val c: Computation.C1U = (r,x0) => {
            r.boxed = null
            f.applyAsLong(n,x0)
          }
          c
        case List(Return(Value.Num(n)), CompiledVar1) =>
          val c: Computation.C2U = (r,x1,_) => {
            r.boxed = null
            f.applyAsLong(n,x1)
          }
          c
        case List(CompiledVar1,CompiledVar0) =>
          val c: Computation.C2U = (r,x1,x0) => {
            r.boxed = null
            f.applyAsLong(x1,x0)
          }
          c
        case List(CompiledVar0,CompiledVar1) =>
          val c: Computation.C2U = (r,x1,x0) => {
            r.boxed = null
            f.applyAsLong(x0,x1)
          }
          c
        case List(arg1: Computation.C2U, arg2: Computation.C2U) =>
          val c: Computation.C2U = (r,x1,x0) =>
            // no need to null out r.boxed, as that will be done by arg1 and arg2
            f.applyAsLong(arg1(r,x1,x0), arg2(r,x1,x0))
          c
        case List(arg1,arg2) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          val x1v = eval(arg1,r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
          val x0v = eval(arg2,r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
          r.boxed = null
          f.applyAsLong(x1v, x0v)
        }
      }
      override def underapply(builtins: Name => Computation)
                             (argCount: Int, substs: Map[Name, Term]): Lambda =
        substs.toList match {
          case List((_,term)) => term match {
            case Term.Compiled2(p: Param) =>
              val n = p.toValue.asInstanceOf[Value.Num].n
              val body: Computation =
                (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
                  r.boxed = null
                  f.applyAsLong(n, x0)
                }
              new Lambda(1, body, Term.Apply(decompiled, term)) {
                def names = self.names drop argCount
              }
            case _ => sys.error("")
          }
          case _ => sys.error("unpossible")
        }
    }
  }
}

