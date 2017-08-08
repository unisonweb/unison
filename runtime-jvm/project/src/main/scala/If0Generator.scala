package org.unisonweb.codegeneration

import java.io.File

object If0Generator {
  def apply(outDir: File): (File, String) =
    (new File(outDir, "If0.scala"), source)

  val N = maxInlineArity

  def source =

   "package org.unisonweb" <>
   "package compilation" <>
   "" <>
   "import Runtime._" <>
   "import Term.Name" <>
   "import annotation.switch" <>
   "" <>
   "object If0 " + {
     "" <> """
    |  def compileIf0(
    |      builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
    |      recursiveVars: Set[Name], currentRec: Option[(Name,Arity)], isTail: Boolean)(
    |      cond: TermC, if0: TermC, ifNot0: TermC): Rt = """.stripMargin + {
        "val compiledCond = Runtime.compile(builtins, cond, boundByCurrentLambda, recursiveVars, currentRec, IsNotTail)" <>
        "val compiledIf0 = Runtime.compile(builtins, if0, boundByCurrentLambda, recursiveVars, currentRec, isTail)" <>
        "val compiledIfNot0 = Runtime.compile(builtins, ifNot0, boundByCurrentLambda, recursiveVars, currentRec, isTail)" <>
        "// todo - partial evaluation, if cond has no free vars" <>
        "(arity(freeVars(e), env(e)): @switch) match " + {
           (0 until N).each { i =>
             s"case $i => class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity${i}(e, ()) " + {
               applySignature(i) + " = " + {
                 "if ({" + eval(i, "cond") + "} == 0.0)" <>
                   tailEval(i, "if0").indent <>
                 "else " + tailEval(i, "ifNot0")
               }.indent <>
               "def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }"
             }.b <>
             "new CIf0(compiledCond, compiledIf0, compiledIfNot0)"
           } <>
           "case n => class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends ArityN(n,e,()) " + {
              "def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }" <>
              "def apply(rec: Rt, args: Array[Slot], r: R) =" <> {
                "if ({ try cond(rec,args,r) catch { case e: TC => loop(e,r) }} == 0.0) if0(rec,args,r)" <>
                "else ifNot0(rec,args,r)"
               }.indent
             }.b <>
             "new CIf0(compiledCond, compiledIf0, compiledIfNot0)".indent
        }.b
      }.b
    }.b
/*
  def compileIf0(
      builtins: String => Rt, e: TermC, boundByCurrentLambda: Option[Set[Name]],
      recursiveVars: Set[Name], currentRec: Option[(Name,Arity)], isTail: Boolean)(
      cond: TermC, if0: TermC, ifNot0: TermC): Rt = {
    val compiledCond = compile(builtins, cond, boundByCurrentLambda, recursiveVars, currentRec, IsNotTail)
    val compiledIf0 = compile(builtins, if0, boundByCurrentLambda, recursiveVars, currentRec, isTail)
    val compiledIfNot0 = compile(builtins, ifNot0, boundByCurrentLambda, recursiveVars, currentRec, isTail)
    // todo - partial evaluation, if cond has no free vars
    arity(freeVars(e), env(e)) match {
      case 0 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity0(e,()) {
          def apply(rec: Rt, r: R) = if ({ try cond(rec,r) catch { case e: TC => loop(e,r) }} == 0.0) if0(rec,r) else ifNot0(rec,r)
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case 1 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity1(e,()) {
          def apply(rec: Rt, x1: D, x1b: Rt, r: R) =
            if ({ try cond(rec,x1,x1b,r) catch { case e: TC => loop(e,r) }} == 0.0) if0(rec,x1,x1b,r)
            else ifNot0(rec,x1,x1b,r)
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case 2 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity2(e,()) {
          def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) =
            if ({ try cond(rec,x1,x1b,x2,x2b,r) catch { case e: TC => loop(e,r) }} == 0.0) if0(rec,x1,x1b,x2,x2b,r)
            else ifNot0(rec,x1,x1b,x2,x2b,r)
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case 3 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity3(e,()) {
          def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, r: R) =
            if ({ try cond(rec,x1,x1b,x2,x2b,x3,x3b,r) catch { case e: TC => loop(e,r) }} == 0.0) if0(rec,x1,x1b,x2,x2b,x3,x3b,r)
            else ifNot0(rec,x1,x1b,x2,x2b,x3,x3b,r)
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case 4 =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends Arity4(e,()) {
          def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, x3: D, x3b: Rt, x4: D, x4b: Rt, r: R) =
            if ({ try cond(rec,x1,x1b,x2,x2b,x3,x3b,x4,x4b,r) catch { case e: TC => loop(e,r) }} == 0.0) if0(rec,x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
            else ifNot0(rec,x1,x1b,x2,x2b,x3,x3b,x4,x4b,r)
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
      case n =>
        class CIf0(cond: Rt, if0: Rt, ifNot0: Rt) extends ArityN(n,e,()) {
          def apply(rec: Rt, args: Array[Slot], r: R) =
            if ({ try cond(rec,args,r) catch { case e: TC => loop(e,r) }} == 0.0) if0(rec,args,r)
            else ifNot0(rec,args,r)
          def bind(env: Map[Name,Rt]) = { cond.bind(env); if0.bind(env); ifNot0.bind(env) }
        }
        new CIf0(compiledCond, compiledIf0, compiledIfNot0)
    }
  }
  */
}
