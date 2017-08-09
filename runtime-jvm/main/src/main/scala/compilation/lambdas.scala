package org.unisonweb.compilation

import org.unisonweb.Term
import org.unisonweb.Term.{Compiled, Lam, Lam1, Name, Term}

class Lambda1(name: Name, e: => Term, compiledBody: Rt) extends Arity1(e) {
  def bind(env: Map[Name,Rt]) = compiledBody.bind(env - name)
  def apply(rec: Rt, x1: D, x1b: Rt, r: R) = compiledBody(rec, x1, x1b, r)
  override def isEvaluated = true
}

class Lambda2(name1: Name, name2: Name, e: => Term, body: => Term, compiledBody: Rt, builtins: String => Rt) extends Arity2(e) {
  var bound: Map[Name,Rt] = Map.empty
  def bind(env: Map[Name,Rt]) = if (env.isEmpty) () else {
    val env2 = env - name1 - name2
    compiledBody.bind(env2)
    bound = bound ++ env2
  }
  override def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
    val rt = toRuntime(x1, x1b)
    Term.betaReduce(name1, Lam(name2)(body))(Compiled(rt)) match {
      case tm@Lam1(name2, body) =>
        val lam = new Lambda1(name2, tm, compile(builtins)(body))
        lam.bind(bound)
        r.boxed = lam
        0.0
    }
  }
  def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = compiledBody(rec, x1, x1b, x2, x2b, r)
  override def isEvaluated = true
}

class Lambda3(name1: Name, name2: Name, name3: Name, e: => Term, body: => Term, compiledBody: Rt, builtins: String => Rt) extends Arity3(e) {
  var bound: Map[Name,Rt] = Map.empty
  def bind(env: Map[Name,Rt]) = if (env.isEmpty) () else {
    val env2 = env - name1 - name2 - name3
    compiledBody.bind(env2)
    bound = bound ++ env2
  }
  override def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
    apply(rec, x2, x2b, r)
    r.boxed(rec, x1, x1b, r) // todo - more direct impl
  }
  override def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
    val rt = toRuntime(x1, x1b)
    Term.betaReduce(name1, Lam(name2, name3)(body))(Compiled(rt)) match {
      case tm@Lam(List(name2, name3), body) =>
        val lam = new Lambda2(name2, name3, tm, body, compile(builtins)(body), builtins)
        lam.bind(bound)
        r.boxed = lam
        0.0
    }
  }
  def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) =
    compiledBody(rec, x1, x1b, x2, x2b, x3, x3b, r)
  override def isEvaluated = true
}

class Lambda4(name1: Name, name2: Name, name3: Name, name4: Name,
  e: => Term, body: => Term, compiledBody: Rt, builtins: String => Rt) extends Arity4(e) {
  var bound: Map[Name,Rt] = Map.empty
  def bind(env: Map[Name,Rt]) = if (env.isEmpty) () else {
    val env2 = env - name1 - name2 - name3
    compiledBody.bind(env2)
    bound = bound ++ env2
  }
  override def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
    val rt1 = toRuntime(x1, x1b)
    val rt2 = toRuntime(x2, x2b)
    Term.betaReduce2(name1, name2, Lam(name3, name4)(body))(Compiled(rt2), Compiled(rt1)) match {
      case tm@Lam(List(name3,name4), body) =>
        val lam = new Lambda2(name3, name4, tm, body, compile(builtins)(body), builtins)
        lam.bind(bound)
        r.boxed = lam
        0.0
    }
  }
  override def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
    val rt1 = toRuntime(x1, x1b)
    val rt2 = toRuntime(x2, x2b)
    val rt3 = toRuntime(x3, x3b)
    Term.betaReduce3(name1, name2, name3, Lam(name4)(body))(Compiled(rt3), Compiled(rt2), Compiled(rt1)) match {
      case tm@Lam(List(name4), body) =>
        val lam = new Lambda1(name4, tm, compile(builtins)(body))
        lam.bind(bound)
        r.boxed = lam
        0.0
    }
  }
  override def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
    val rt = toRuntime(x1, x1b)
    Term.betaReduce(name1, Lam(name2, name3, name4)(body))(Compiled(rt)) match {
      case tm@Lam(List(name2, name3, name4), body) =>
        val lam = new Lambda3(name2, name3, name4, tm, body, compile(builtins)(body), builtins)
        lam.bind(bound)
        r.boxed = lam
        0.0
    }
  }
  def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) =
    compiledBody(rec, x1, x1b, x2, x2b, x3, x3b, x4, x4b, r)
  override def isEvaluated = true
}

class LambdaN(names: Array[Name], e: => Term, body: => Term, compiledBody: Rt, builtins: String => Rt)
  extends ArityN(names.length, e) {
  var bound: Map[Name,Rt] = Map.empty
  def bind(env: Map[Name,Rt]) = if (env.isEmpty) () else {
    val env2 = env -- names
    compiledBody.bind(env2)
    bound = bound ++ env2
  }
  override def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
    val rt = toRuntime(x1, x1b)
    Term.betaReduce(names(0), Lam(names.drop(1):_*)(body))(Compiled(rt)) match {
      case tm@Lam(names, body) => names match {
        case List(name1,name2,name3,name4) =>
          val lam = new Lambda4(name1,name2,name3,name4, tm, body, compile(builtins)(body), builtins)
          lam.bind(bound)
          r.boxed = lam
          0.0
        case _ =>
          val lam = new LambdaN(names.toArray, tm, body, compile(builtins)(body), builtins)
          lam.bind(bound)
          r.boxed = lam
          0.0
      }
    }
  }
  override def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
    apply(rec, x2, x2b, r)
    r.boxed(rec, x1, x1b, r) // todo - more direct impl
  }
  override def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) = {
    apply(rec, x3, x3b, r)
    r.boxed(rec, x2, x2b, r)
    r.boxed(rec, x1, x1b, r)
  }
  override def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) = {
    apply(rec, x4, x4b, r)
    r.boxed(rec, x3, x3b, r)
    r.boxed(rec, x2, x2b, r)
    r.boxed(rec, x1, x1b, r)
  }
  def apply(rec: Rt, args: Array[Slot], r: R) =
    if (args.length == names.length) compiledBody(rec, args, r)
    else if (args.length < names.length) {
      var i = args.length
      var rt: Rt = this
      while (i > 0) {
        val slot = args(i-1)
        rt(rec, slot.unboxed, slot.boxed, r)
        rt = r.boxed
        i -= 1
      }
      0.0
    }
    else sys.error("LambdaN overapplication")

  override def isEvaluated = true
}