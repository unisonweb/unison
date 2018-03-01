package org.unisonweb.codegeneration

object ValueGenerator extends OneFileGenerator("Value.scala") {
  private def applyRBoxed(args: String): String =
    match1("(r.boxed: @unchecked)") { caseInline("lambda: Lambda") { s"lambda(lambda, " + args + commaIf(args.length) + "r) "}}
  private def applyRBoxed(i: Int): String = applyRBoxed(xsArgs(i))
  private def applyRBoxedN: String = applyRBoxed("xs")

  def source =
    "package org.unisonweb.compilation" <>
    "" <>
    "import org.unisonweb.ABT" <>
    "import org.unisonweb.Term" <>
    "import org.unisonweb.Term.{Term, Name}" <>
    "" <>
    b("sealed abstract class Value") {
      "def decompile: Term" <>
      "def apply(r: R): D"
    } <<>>
    b("object Value") { "def apply(d: D, v: Value): Value = if (v eq null) Num(d) else v" } <<>>
    b("case class Num(d: D) extends Value") {
      "def decompile = Term.Num(d)" <>
      "def apply(r: R) = d"
    } <<>>
    "// abstract class Data extends Value" <<>>
    b("case class Ref(name: Name, var value: Value = null) extends Value") {
      "def decompile = value.decompile" <>
      "def apply(r: R) = value(r)" <>
      "// want equality to be based on pointer equality" <>
      "override def equals(a: Any) = this eq a.asInstanceOf[AnyRef]" <>
      "override def hashCode = System.identityHashCode(this)"
    } <<>>
    b("abstract class Lambda extends Value") {
      "def arity: Int" <>
      "def apply(r: R) = { r.boxed = this; 0.0 }" <>
      (0 to maxInlineArgs).each(applySignature) <>
      applyNSignature
    } <<>>
    b("object Lambda") {
      bEq("def unapply(value: Value): Option[(Lambda, Arity)]") {
        `match`("value") {
          "case l: Lambda => Some((l, l.arity))" <>
          "case _ => None"
        }
      }
    } <<>>
    generateLambdas

  val generateLambdas: String = {
    def N = maxInlineArgs
    def lambdaCtorArgName(i: Int): String = s"name$i"
    def lambdaCtorArgs(i: Int): String =
      (1 to i).commas(i => lambdaCtorArgName(i) + ": Name")

    // definitions for Lambda{x}.apply(fixed arity)
    def fixedApplyDefs(i: Int) =
      0 to N each {
        case 0 =>
          "// apply 0 arguments / no-op" <>
          s"${applySignature(0)} = { r.boxed = this; 0.0 }" <> ""

        case j if j < i =>
          val unboundNames = (j until i).commas(k => s"name${k + 1}")
          s"// partial application: $j of $i arguments" <>
          bEq(applySignature(j)) {
            `match`("ABT.substs(Map(" +
              (0 until j).commas { k => s"\n(name${k + 1}, Term.Compiled(Value(x${j - 1 - k}, x${j - 1 - k}b)))" }.indent <>
              ")" + (j+1 to i).map(" - name" + _).mkString + ")(unTermC(body))") {
                `case`("body")(
                  s"val lam2 = Term.Lam($unboundNames)(body)" <>
                  // todo maybe not call root compile if needed for speed
                  "r.boxed = compile(checkedAnnotateBound(lam2)) match {" <>
                  "  case Return(v) => v" <>
                  "  case _ => sys.error(\"a partially-applied lambda should produce a lambda.\")" <>
                  "}" <>
                  "0.0"
                )
              }
          }.<>|

        case j if j == i =>
          s"// exact application: all $i arguments" <>
          s"${applySignature(i)} =" <>
            tailEval(i, "compiledBody").indent <> ""

        case j /* j > i */ =>
          s"// over-application: all $i arguments + ${j-i} more" <>
          bEq(applySignature(j)) {
            tailEval(i, "apply") <>
            applyRBoxed(xArgs(i, j-i))
          } <> ""
      }

    def fixedOverapplyN(expected: Int): String =
      s"// over-application with ${N+1} or more args: all $expected arguments + at least ${N+1-expected} more" <>
        bEq(applyNSignature) {
          s"apply(rec, ${xsArgs(expected)}, r)" <>
          `match`(s"xs.drop($expected)") {
            "case xs => " + switch("xs.length") {
              ((N+1-expected) to N).each { i => caseInline(i)(applyRBoxed(i)) } <>
              s"case n if n > $N => $applyRBoxedN"
            }
          }
        } <> ""

    s"/** Specialized Lambda of 1 parameter */" <>
    includeIf(N) {
      b(s"class Lambda1(name: Name, compiledBody: Computation, decompileIt: => Term) extends Lambda") {
        s"def decompile = decompileIt" <>
          s"def arity = 1" <>
          fixedApplyDefs(1) <>
          fixedOverapplyN(1)
      }.<<>>|
    } <>
    (2 to N).eachNL { i =>
      s"/** Specialized Lambda of $i parameters */" <>
      b(s"class Lambda$i(${lambdaCtorArgs(i)}, compiledBody: Computation, decompileIt: => Term)(body: TermC, compile: TermC => Computation) extends Lambda") {
        s"def decompile = decompileIt" <>
        s"def arity = $i" <>
          fixedApplyDefs(i) <>
          fixedOverapplyN(i)
      }
    } <>
    "" <>
    s"/** Lambda with ${N+1} or more parameters */" <>
    b("class LambdaN(argNames: Array[Name], compiledBody: Computation, decompileIt: => Term)(body: TermC, compile: TermC => Computation) extends Lambda") {
      s"def decompile = decompileIt" <>
      "def arity = argNames.length" <>
      (0 to N).eachNL {
        case 0 =>
          "// apply 0 arguments / no-op" <>
          s"${applySignature(0)} = { r.boxed = this; 0.0 }"

        case j =>
          s"// partial application: $j of ${N+1}+ arguments; ${N+1-j}+ additional arguments needed" <>
          bEq(applySignature(j)) {
            s"val unboundNames = argNames.drop($j)" <>
            s"assert(unboundNames.length >= ${N+1-j})" <<>>
            `match`("ABT.substs(Map(" +
              (0 until j).commas { k => s"\n(argNames($k), Term.Compiled(Value(x${j - 1 - k}, x${j - 1 - k}b)))" }.indent <>
              s") -- unboundNames)(unTermC(body))") {
                `case`("body") {
                  "val lam2 = Term.Lam(unboundNames: _*)(body)" <>
                  "r.boxed = compile(checkedAnnotateBound(lam2)) match {" <>
                  "  case Return(v) => v" <>
                  "  case _ => sys.error(\"a partially-applied lambda should produce a lambda.\")" <>
                  "}" <>
                  "0.0"
                }
            }
          }
      } <>
      "" <>
      bEq(applyNSignature) {
        "if (xs.length == argNames.length) compiledBody(rec, xs, r)" <>
        b("else if (xs.length < argNames.length)") {
          "// under-application" <>
          "val unboundNames = argNames.drop(xs.length)" <>
          `match`(b("ABT.substs((0 until xs.length).map") {
            "i => val x = xs(xs.length - 1 - i); (argNames(i), Term.Compiled(Value(x.unboxed, x.boxed)))"
          } + ".toMap -- unboundNames)(unTermC(body))") {
            `case`("body") {
              "val lam2 = Term.Lam(unboundNames: _*)(body)" <>
              "r.boxed = compile(checkedAnnotateBound(lam2)) match {" <>
              "  case Return(v) => v" <>
              "  case _ => sys.error(\"a partially-applied lambda should produce a lambda.\")" <>
              "}" <>
              "0.0"
            }
          }
        } <>
        b("else") {
          "// over-application" <>
          "apply(rec, xs.take(argNames.length), r)" <>
          match1("xs.drop(argNames.length)") {
            caseInline("xs") {
              switch("xs.length") {
                (1 to N).each { i => caseInline(i)(applyRBoxed(xsArgs(i))) } <>
                s"case n if n > $N => $applyRBoxedN"
              }
            }
          }
        }
      }
    }
  }
}

