package org.unisonweb.codegeneration

import java.io.File

object CompileVarGenerator {
  def apply(outDir: File): (File, String) =
    (new File(outDir, "compileVar.scala"), source)

  val N = maxInlineArity

  def source: String =
    s"""package org.unisonweb.compilation
       |
       |import org.unisonweb.Term.Name
       |
       |object compileVar extends ((Name, TermC, Boolean) => Rt) {
       |  def apply(name: Name, e: TermC, compileAsFree: Boolean): Rt =
       |    if (compileAsFree) new Rt {
       |      var rt: Rt = null
       |      def arity = rt.arity
       |${ (0 to N).each { i => applySignature(i) + " = " + tailEval(i, "rt")}.indent }
       |      def apply(rec: Rt, args: Array[Slot], r: R) = rt(rec,args,r)
       |      // if (rt eq null) throw new InfiniteLoopError(name)
       |      // todo : possibly try / catch NPEs
       |      override def bind(env: Map[Name,Rt]) = env.get(name) match {
       |        case Some(rt2) => rt = rt2
       |        case _ => () // not an error, just means that some other scope will bind this free var
       |      }
       |      override def isEvaluated = !(rt eq null)
       |      // let rec loop = loop; loop
       |      // let rec ping = pong; pong = ping; ping
       |      // let rec ping x = pong (x + 1); pong x = ping (x + 1); ping
       |      def decompile = if (rt eq null) unTermC(e) else rt.decompile
       |    }
       |    else env(e).indexOf(name) match {
       |      case -1 => sys.error("unknown variable: " + name)
       |      case i => lookupVar(i, name, unTermC(e))
       |    }
       |}
     """.stripMargin
}
