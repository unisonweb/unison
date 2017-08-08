package org.unisonweb.codegeneration

import java.io.File

object StaticCallGenerator {
  def apply(outDir: File): (File, String) =
    (new File(outDir, "StaticCall.scala"), source)

  def source =
    s"""package org.unisonweb
       |package compilation
       |
       |import Runtime._
       |import Term.{Name,Term}
       |import annotation.switch
       |
       |object StaticCall {
       |
       |  def staticCall(fn: Rt, args: Array[Rt], decompile: Term, isTail: Boolean): Rt =
       |    if (isTail) staticTailCall(fn, args, decompile)
       |    else staticNonTailCall(fn, args, decompile)
       |
       |  def staticRecCall(args: Array[Rt], decompile: Term, isTail: Boolean): Rt =
       |    if (isTail) staticRecTailCall(args, decompile)
       |    else staticRecNonTailCall(args, decompile)
       |
       |${ indent(1, List(staticRecNonTail, staticRecTailCall, staticTailCall, staticNonTailCall) map makeDefinition mkString("\n"))}
       |}
      """.stripMargin

  case class Parts(
    decl: String,
    scalabug: String,
    bind: String,
    fixedStack: Int => String,
    varStack: String
  )

  def makeDefinition(t: Parts): String =
    s"""${t.decl} = {
       |  val arity = args.map(_.arity).max
       |${ indent(1, t.scalabug) }
       |  trait A0 { self: Rt => def bind(env: Map[Name,Rt]) = ${t.bind} }
       |  (arity: @switch) match {
       |${indent(2, 0 to maxInlineArity map caseFixedStack(t.fixedStack, t.varStack))}
       |    case n => (args.length: @switch) match {
       |${indent(3, 1 to maxInlineArity map caseFixedArgsVarStack(t.fixedStack))}
       |${indent(3, caseVarArgsVarStack(t.varStack))}
       |    }
       |  }
       |}
       |""".stripMargin

  def staticRecNonTail = Parts(
    decl = "def staticRecNonTailCall(args: Array[Rt], decompile: Term): Rt",
    scalabug = "val args2 = args",
    bind = "args2.foreach(_.bind(env))",
    fixedStack = argc => s"rec(${"rec" +: xRevArgs(argc) :+ "r" mkString(",")})",
    varStack = "rec(rec, slots, r)"
  )

  def staticRecTailCall = Parts(
    decl = "def staticRecTailCall(args: Array[Rt], decompile: Term): Rt",
    scalabug = staticRecNonTail.scalabug,
    bind = staticRecNonTail.bind,
    fixedStack = argc => s"selfTailCall(${xRevArgs(argc) :+ "r" mkString(",")})",
    varStack = "selfTailCall(slots, r)"
  )

  def staticTailCall = Parts(
    decl = "def staticTailCall(fn: Rt, args: Array[Rt], decompile: Term): Rt",
    scalabug = "val fn2 = fn; val args2 = args",
    bind = "{ fn2.bind(env); args2.foreach(_.bind(env)) }",
    fixedStack = argc => s"tailCall(${"fn" +: xRevArgs(argc) :+ "r" mkString(",")})",
    varStack = "tailCall(fn, slots, r)"
  )

  def staticNonTailCall = Parts(
    decl = "def staticNonTailCall(fn: Rt, args: Array[Rt], decompile: Term): Rt",
    scalabug = staticTailCall.scalabug,
    bind = staticTailCall.bind,
    fixedStack = argc => s"fn(${"fn" +: xRevArgs(argc) :+ "r" mkString(",")})",
    varStack = "fn(fn, slots, r)"
  )


  /** evaluates `arg$i` with fixed stack depth */
  def evaluateArgFixed(stackDepth: Int)(i: Int): String = {
    s"val ${xArg0(i)} = { try arg$i(${"rec" +: aArgs(stackDepth) :+ "r" mkString(",")}) catch { case e: TC => loop(e,r) }}; " +
      s"val ${xArgB0(i)} = r.boxed"
  }

  /** evaluates `arg$i` with arbitrary stack depth */
  def evaluateArgArbitrary(i: Int): String = {
    s"val ${xArg0(i)} = { try arg$i(rec, args, r) catch { case e: TC => loop(e,r) }}; " +
      s"val ${xArgB0(i)} = r.boxed"
  }

  /** creates cases for fixed stack depth */
  def caseFixedStack(argCountToBody: Int => String, slotsBody: String)(stackDepth: Int): String = {
    s"""case $stackDepth => (args.length: @switch) match {
       |${ indent(1, 1 to maxInlineArity map caseFixedArgsFixedStack(stackDepth)(argCountToBody))}
       |${ indent(1, caseVarArgsFixedStack(stackDepth)(slotsBody)) }
       |}
       |""".stripMargin
  }

  /** creates a `case` for handling `argCount` args:
    * unpacks `argCount` arg variables from an `args` seq,
    * evaluates the arguments, and then includes the `body`
    * which presumably does something with the evaluated arguments.
    */
  def caseFixedArgsFixedStack(stackDepth: Int)(argCountToBody: Int => String)(argCount: Int): String = {
    s"""case $argCount =>
       |${indent(1, 0 until argCount map { i => s"val arg$i = args($i)" }) }
       |  new Arity$stackDepth(decompile) with A0 { def apply(${"rec: Rt" +: aParams(stackDepth) :+ "r: R" mkString(", ")}) = {
       |${  indent(2, 0 until argCount map evaluateArgFixed(stackDepth)) }
       |${  indent(2, argCountToBody(argCount)) }
       |  }}
       |""".stripMargin
  }

  def caseFixedArgsVarStack(argCountToBody: Int => String)(argCount: Int): String = {
    s"""case $argCount =>
       |${indent(1, 0 until argCount map { i => s"val arg$i = args($i)" })}
       |  new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = {
       |${  indent(2, 0 until argCount map evaluateArgArbitrary)}
       |${  indent(2, argCountToBody(argCount)) }
       |  }}
       |""".stripMargin
  }

  def caseVarArgsFixedStack(stackDepth: Int)(body: String): String = {
    s"""case _ => // n args, $stackDepth stack depth
       |  new Arity$stackDepth(decompile) with A0 { def apply(${"rec: Rt" +: aParams(stackDepth) :+ "r: R" mkString (", ")}) = {
       |    val slots = new Array[Slot](args.length)
       |    var i = 0
       |    while (i < slots.length) {
       |      val slot = slots(slots.length - 1 - i)
       |      slot.unboxed = { try args(i)(${"rec" +: aArgs(stackDepth) :+ "r" mkString (",")}) catch { case e: TC => loop(e,r) }}
       |      slot.boxed = r.boxed
       |      i += 1
       |    }
       |${ indent(2, body) }
       |  }}
       |""".stripMargin
  }

  def caseVarArgsVarStack(body: String): String = {
    s"""case _ =>
       |  new ArityN(n, decompile) with A0 { def apply(rec: Rt, args0: Array[Slot], r: R) = {
       |    val slots = new Array[Slot](args.length)
       |    var i = 0
       |    while (i < slots.length) {
       |      val slot = slots(slots.length - 1 - i)
       |      slot.unboxed = { try args(i)(rec, args0, r) catch { case e: TC => loop(e,r) }}
       |      slot.boxed = r.boxed
       |      i += 1
       |    }
       |${ indent(2, body) }
       |  }}
       |""".stripMargin
  }
}
