package org.unisonweb

import org.unisonweb.Term.{Name, Term}
import org.unisonweb.compilation2.Value.Lambda
import org.unisonweb.compilation2._

object Lib2 {

  val builtins: Name => Computation =
    Map[Name, NumericBinOp](
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
        name -> Return(builtin2(term, f), term)
    } ++ Map[Name, NumericUnaryOp](
      ("not", b => if (b == False) True else False),
      ("negate", x => -x)
    ).map {
      case (name, f) =>
        val term = Term.Builtin(name)
        name -> Return(builtin1(term, f), term)
    }

  @inline def boolToNum(b: Boolean): U = if (b) True else False

  trait NumericUnaryOp {
    def apply(n1: U): U
  }

  trait NumericBinOp {
    def apply(n1: U, n2: U): U
  }

  def builtin1(decompiled: Term, f: NumericUnaryOp): Lambda = {
    val body = new Computation(null) {
      def apply(r: R, rec: Lambda, top: StackPtr,
                stackU: Array[U], x1: U, x0: U,
                stackB: Array[B], x1b: B, x0b: B): U = {
        r.boxed = null
        f(x0)
      }
    }
    new Lambda(1, body, decompiled)
  }

  def builtin2(decompiled: Term, f: NumericBinOp): Lambda = {
    val body = new Computation(null) {
      def apply(r: R, rec: Lambda, top: StackPtr,
                stackU: Array[U], x1: U, x0: U,
                stackB: Array[B], x1b: B, x0b: B): U = {
        r.boxed = null
        f(x1, x0)
      }
    }

    new Lambda(2, body, decompiled) {
      override def underapply(builtins: Name => Computation)(argCount: Int, substs: Map[Name, Term]): Lambda =
        substs.toList match {
          case List((name,term)) => term match {
            case Term.Compiled2(p: Param) =>
              val n = p.toValue.asInstanceOf[Value.Num].n
              new Lambda(1, new Computation(null) {
                def apply(r: R, rec: Lambda, top: StackPtr,
                          stackU: Array[U], x1: U, x0: U,
                          stackB: Array[B], x1b: B, x0b: B): U = {
                  r.boxed = null
                  f(n, x0)
                }
              }, Term.Apply(decompiled, term))
            case _ => sys.error("")
          }
          case _ => sys.error("unpossible")
        }
    }
  }
}
