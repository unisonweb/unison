//package org.unisonweb.codegeneration
//
//object FunctionApplicationGenerator extends OneFileGenerator("FunctionApplication.scala") {
//  def source =
//    "package org.unisonweb.compilation" <>
//    "" <>
//    "import org.unisonweb.Term.Term" <>
//    "" <>
//    "trait FunctionApplication " + {
//      "def staticCall(fn: Lambda, args: Array[Computation], decompile: Term, isTail: Boolean): Computation = " + {
//        "if (isTail) staticTailCall(fn, args, decompile)" <>
//        "else staticNonTailCall(fn, args, decompile)"
//      }.indent <>
//      "" <>
//      "def staticRecCall(args: Array[Computation], decompile: Term, isTail: Boolean): Computation = " + {
//        "if (isTail) staticRecTailCall(args, decompile)" <>
//        "else staticRecNonTailCall(args, decompile)"
//      }.indent <>
//      "" <>
//      "def staticRecNonTailCall(args: Array[Computation], decompile: Term): Computation = " + {
//        "val stackSize = args.map(_.stackSize).max" <>
//        "(stackSize: @annotation.switch) match " + {
//          (0 to N).each { stackSize =>
//            s"case $stackSize => (args.length: @annotation.switch) match " + {
//              (1 to N).each { argCount =>
//                s"case $argCount =>" <> {
//                  (0 until argCount).each { i => s"val arg$i = args(i)" } <>
//                  s"class StaticRecCallS${stackSize}A${argCount} extends Computation${stackSize}(decompile) " + {
//                    applySignature(stackSize) + " = " + {
//                      (0 until argCount).each(i => s"val e$i = { ${eval(stackSize, s"arg$i")} }; val e${i}b = r.boxed") <>
//                      s"rec(rec, " + (argCount-1 to 0 by -1).commas(i => s"e$i, e${i}b") + commaIf(N-1) + "r)"
//                    }.b
//                  }.b
//                  s"new StaticRecCallS${stackSize}A${argCount}"
//                }.indent
//              } <>
//              "case argCount =>" <> {
//                  //todo
//              }.indent
//            }.b
//          } <>
//          "case stackSize => (args.length: @annotation.switch) match " + {
//            (1 to N).each { argCount =>
//              s"case $argCount =>" <> {
//                (0 until argCount).each { i => s"val arg$i = args($i)" } <>
//                s"class StaticRecCallSNA$argCount extends ComputationN(stackSize, decompile)" + {
//                  // todo
//                }.b <>
//                s"new StaticRecCallSNA$argCount"
//              }.indent
//            }.b
//          }.b
//        }.b
//      }.b
//
//    }.b
//
//    //   def caseFixedArgsVarStack(argCountToBody: Int => String)(argCount: Int): String = {
//    //     s"case $argCount =>" <> {
//    //       (0 until argCount).each { i => s"val arg$i = args($i)" } <>
//    //       "new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = " + {
//    //         (0 until argCount).each(evaluateArgArbitrary) <>
//    //         argCountToBody(argCount)
//    //       }.b + "}"
//    //     }.indent
//    //   }
//
//    //   def staticRecNonTail = Parts(
//    //     decl = "def staticRecNonTailCall(args: Array[Rt], decompile: Term): Rt",
//    //     scalabug = "val args2 = args",
//    //     bind = "args2.foreach(_.bind(env))",
//    //     fixedStack = argc => "rec(rec, " + (argc-1 to 0 by -1).commas(i => s"e$i, e${i}b") + commaIf(N-1) + "r)",
//    //     varStack = "rec(rec, slots, r)"
//    //   )
//
//    //   def makeDefinition(t: Parts): String =
//    //     s"${t.decl} = " + {
//    //       "val arity = args.map(_.arity).max" <>
//    //       t.scalabug <>
//    //       s"trait A0 { self: Rt => def bind(env: Map[Name,Rt]) = ${t.bind} }" <>
//    //       "(arity: @switch) match " + {
//    //         (0 to N).each(caseFixedStack(t.fixedStack, t.varStack)) <>
//    //         "case n => (args.length: @switch) match " + {
//    //           (1 to N).each(caseFixedArgsVarStack(t.fixedStack)) <>
//    //           caseVarArgsVarStack(t.varStack)
//    //         }.b
//    //       }.b
//    //     }.b
//
//
//
//
////   def source =
////     s"""package org.unisonweb
////        |package compilation
////        |
////        |import Term.{Name,Term}
////        |import annotation.switch
////        |
////        |object StaticCall {
////        |
////        |  def staticCall(fn: Rt, args: Array[Rt], decompile: Term, isTail: Boolean): Rt =
////        |    if (isTail) staticTailCall(fn, args, decompile)
////        |    else staticNonTailCall(fn, args, decompile)
////        |
////        |  def staticRecCall(args: Array[Rt], decompile: Term, isTail: Boolean): Rt =
////        |    if (isTail) staticRecTailCall(args, decompile)
////        |    else staticRecNonTailCall(args, decompile)
////        |
////        |${ indent(1, List(staticRecNonTail, staticRecTailCall, staticTailCall, staticNonTailCall) map makeDefinition mkString "\n")}
////        |}
////       """.stripMargin
////
////   case class Parts(
////     decl: String,
////     scalabug: String,
////     bind: String,
////     fixedStack: Int => String,
////     varStack: String
////   )
////
////   def makeDefinition(t: Parts): String =
////     s"${t.decl} = " + {
////       "val arity = args.map(_.arity).max" <>
////       t.scalabug <>
////       s"trait A0 { self: Rt => def bind(env: Map[Name,Rt]) = ${t.bind} }" <>
////       "(arity: @switch) match " + {
////         (0 to N).each(caseFixedStack(t.fixedStack, t.varStack)) <>
////         "case n => (args.length: @switch) match " + {
////           (1 to N).each(caseFixedArgsVarStack(t.fixedStack)) <>
////           caseVarArgsVarStack(t.varStack)
////         }.b
////       }.b
////     }.b
////
////   def staticRecNonTail = Parts(
////     decl = "def staticRecNonTailCall(args: Array[Rt], decompile: Term): Rt",
////     scalabug = "val args2 = args",
////     bind = "args2.foreach(_.bind(env))",
////     fixedStack = argc => "rec(rec, " + (argc-1 to 0 by -1).commas(i => s"e$i, e${i}b") + commaIf(N-1) + "r)",
////     varStack = "rec(rec, slots, r)"
////   )
////
////   def staticRecTailCall = Parts(
////     decl = "def staticRecTailCall(args: Array[Rt], decompile: Term): Rt",
////     scalabug = staticRecNonTail.scalabug,
////     bind = staticRecNonTail.bind,
////     fixedStack = argc => "selfTailCall(" + (argc-1 to 0 by -1).commas(i => s"e$i, e${i}b") + commaIf(N-1) + "r)",
////     varStack = "selfTailCall(slots, r)"
////   )
////
////   def staticTailCall = Parts(
////     decl = "def staticTailCall(fn: Rt, args: Array[Rt], decompile: Term): Rt",
////     scalabug = "val fn2 = fn; val args2 = args",
////     bind = "{ fn2.bind(env); args2.foreach(_.bind(env)) }",
////     fixedStack = argc => "tailCall(fn, " + (argc-1 to 0 by -1).commas(i => s"e$i, e${i}b") + commaIf(N-1) + "r)",
////     varStack = "tailCall(fn, slots, r)"
////   )
////
////   def staticNonTailCall = Parts(
////     decl = "def staticNonTailCall(fn: Rt, args: Array[Rt], decompile: Term): Rt",
////     scalabug = staticTailCall.scalabug,
////     bind = staticTailCall.bind,
////     fixedStack = argc => "fn(fn, " + (argc-1 to 0 by -1).commas(i => s"e$i, e${i}b") + commaIf(N-1) + "r)",
////     varStack = "fn(fn, slots, r)"
////   )
////
////
////   /** evaluates `arg$i` with fixed stack depth */
////   def evaluateArgFixed(stackDepth: Int)(i: Int): String = {
////     s"val e$i = { ${eval(stackDepth, s"arg$i")} }; val e${i}b = r.boxed"
////   }
////
////   /** evaluates `arg$i` with arbitrary stack depth */
////   def evaluateArgArbitrary(i: Int): String = {
////     s"val e$i = { try arg$i(rec, args, r) catch { case e: TC => loop(e,r) }}; val e${i}b = r.boxed"
////   }
////
////   /** creates cases for fixed stack depth */
////   def caseFixedStack(argCountToBody: Int => String, slotsBody: String)(stackDepth: Int): String = {
////     s"case $stackDepth => (args.length: @switch) match " + {
////       (1 to N).each(caseFixedArgsFixedStack(stackDepth)(argCountToBody)) <>
////         caseVarArgsFixedStack(stackDepth)(slotsBody)
////     }.b
////   }
////
////   /** creates a `case` for handling `argCount` args:
////     * unpacks `argCount` arg variables from an `args` seq,
////     * evaluates the arguments, and then includes the `body`
////     * which presumably does something with the evaluated arguments.
////     */
////   def caseFixedArgsFixedStack(stackDepth: Int)(argCountToBody: Int => String)(argCount: Int): String = {
////     s"case $argCount =>" <> {
////       (0 until argCount).each { i => s"val arg$i = args($i)" } <>
////       s"new Arity$stackDepth(decompile) with A0 " + {
////         applySignature(stackDepth) + " = " + {
////           (0 until argCount).each(evaluateArgFixed(stackDepth)) <>
////           argCountToBody(argCount)
////         }.b
////       }.b
////     }.indent
////   }
////
////   def caseFixedArgsVarStack(argCountToBody: Int => String)(argCount: Int): String = {
////     s"case $argCount =>" <> {
////       (0 until argCount).each { i => s"val arg$i = args($i)" } <>
////       "new ArityN(n, decompile) with A0 { def apply(rec: Rt, args: Array[Slot], r: R) = " + {
////         (0 until argCount).each(evaluateArgArbitrary) <>
////         argCountToBody(argCount)
////       }.b + "}"
////     }.indent
////   }
////
////   def caseVarArgsFixedStack(stackDepth: Int)(body: String): String = {
////     s"case _ => // n args, $stackDepth stack depth" <> {
////       s"new Arity$stackDepth(decompile) with A0 " + {
////         applySignature(stackDepth) + " = " + {
////           "val slots = new Array[Slot](args.length)" <>
////           "var i = 0" <>
////           "while (i < slots.length) " + {
////             "val slot = slots(slots.length - 1 - i)" <>
////             s"slot.unboxed = ${eval(stackDepth, "args(i)")}" <>
////             "slot.boxed = r.boxed" <>
////             "i += 1"
////           }.b <>
////           body
////         }.b
////       }.b
////     }.indent
////   }
////
////   def caseVarArgsVarStack(body: String): String = {
////     "case _ =>" <> {
////       "new ArityN(n, decompile) with A0 { def apply(rec: Rt, args0: Array[Slot], r: R) = " <> {
////         "val slots = new Array[Slot](args.length)" <>
////         "var i = 0" <>
////         "while (i < slots.length) " + {
////           "val slot = slots(slots.length - 1 - i)" <>
////           "slot.unboxed = { try args(i)(rec, args0, r) catch { case e: TC => loop(e,r) }}" <>
////           "slot.boxed = r.boxed" <>
////           "i += 1"
////         }.b <>
////         body
////       }.b + "}"
////     }.indent
////   }
//// }
