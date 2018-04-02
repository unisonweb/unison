package org.unisonweb

import org.unisonweb.Term.{Name, Term}
import org.unisonweb.compilation2.Value.Lambda
import org.unisonweb.compilation2._

object Lib2 {

  val builtins: Name => Computation =
    List[(Name, NumericBinOp)](
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
        name -> Return(builtin2(term, List("x1", "x2"), f), term)
    }.toMap ++ Map[Name, NumericUnaryOp](
      ("not", b => if (b == False) True else False),
      ("negate", x => -x)
    ).map {
      case (name, f) =>
        val term = Term.Builtin(name)
        name -> Return(builtin1(term, "x", f), term)
    }.toMap

  @inline def boolToNum(b: Boolean): U = if (b) True else False

  abstract class NumericUnaryOp {
    def apply(n1: U): U
  }

  abstract class NumericBinOp {
    def apply(n1: U, n2: U): U
  }

  def builtin1(decompiled: Term, n: Name, f: NumericUnaryOp): Lambda = {
    val body: Computation = (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
      r.boxed = null
      f(x0)
    }
    val ns = List(n)
    new Lambda(1, body, decompiled) { def names = ns }
  }

  def builtin2(decompiled: Term, ns: List[Name], f: NumericBinOp): Lambda = {
    val body: Computation = (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
      r.boxed = null
      f(x1, x0)
    }

    new Lambda(2, body, decompiled) { self =>
      def names = ns
      override def saturatedNonTailCall(args: List[Computation]) = args match {
        case List(Return(Value.Num(n1)), Return(Value.Num(n2))) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          r.boxed = null
          f(n1, n2)
        }
        case List(CompiledVar0,Return(Value.Num(n))) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          r.boxed = null
          f(x0, n)
        }
        case List(CompiledVar1,Return(Value.Num(n))) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          r.boxed = null
          f(x1, n)
        }
        case List(Return(Value.Num(n)), CompiledVar0) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          r.boxed = null
          f(n, x0)
        }
        case List(Return(Value.Num(n)), CompiledVar1) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          r.boxed = null
          f(n, x1)
        }
        case List(CompiledVar1,CompiledVar0) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          r.boxed = null
          f(x1, x0)
        }
        case List(CompiledVar0,CompiledVar1) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          r.boxed = null
          f(x0, x1)
        }
        case List(arg1,arg2) => (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
          val x1v = eval(arg1,r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
          val x0v = eval(arg2,r,rec,top,stackU,x1,x0,stackB,x1b,x0b)
          r.boxed = null
          f(x1v, x0v)
        }
      }
      override def underapply(builtins: Name => Computation)
                             (argCount: Int, substs: Map[Name, Term]): Lambda =
        substs.toList match {
          case List((_,term)) => term match {
            case Term.Compiled2(p: Param) =>
              val n = p.toValue.asInstanceOf[Value.Num].n
              val body: Computation = (r,rec,top,stackU,x1,x0,stackB,x1b,x0b) => {
                r.boxed = null
                f(n, x0)
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
