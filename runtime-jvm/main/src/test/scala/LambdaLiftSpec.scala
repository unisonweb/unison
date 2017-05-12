package org.unisonweb

import org.scalacheck._
import org.scalacheck.Prop._
import Term._
object LambdaLiftSpec extends Properties("Term.lambdaLift") { 
  
  implicit def toVar(s: String) = Var(s)
  implicit def toNum(n: Int) = Num(n)
  
  property("no lambda") = {
    // let x = 1
    //     y = x
    //     y
    val term = Let(
      "x" -> 1,
      "y" -> "x")(
      "y") 
    // let x = 1
    //     y x = x
    //     y x
    val liftedTerm = Let(
      "x" -> 1, 
      "y" -> Lam("x")("x"))(
      Var("y")("x")
    )
    Term.lambdaLift(term) ?= liftedTerm
  }
  
  property("simple lambda") = {
    // let x = 1
    //     y a = x a
    //     y 5
    val term = Let(
      "x" -> 1,
      "y" -> Lam("a")(Apply("x","a")))(
      Var("y")(5)
    )
    // let x = 1
    //     y x a = x a 
    //     y x 5
    val liftedTerm = Let(
      "x" -> 1, 
      "y" -> Lam("x","a")(Apply("x","a")))(
      (Var("y")("x", 5))
    )
    Term.lambdaLift(term) ?= liftedTerm
  }

  property("noop (1)") = {
    val id = Lam1("x")("x")
    Term.lambdaLift(id) ?= id 
  }
  
  property("noop (2)") = {
    val e = Let("x" -> 1.0, "y" -> 2.0)("y")
    Term.lambdaLift(e) ?= e
  }
  
  def isOk(t: Term): Boolean = ???
}

