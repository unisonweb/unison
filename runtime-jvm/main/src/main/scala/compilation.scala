package org.unisonweb

import org.unisonweb.ABT.AnnotatedTerm
import org.unisonweb.Term.{Name, Term}
import org.unisonweb.util.Lazy

package compilation {
  case class Slot(var unboxed: D, var boxed: Value)

  case class CurrentRec(get: Option[(Name, Arity)]) extends AnyVal {
    def isEmpty = get.isEmpty
    def contains(name: Name): Boolean = get.exists(_._1 == name)
    def contains(name: Name, arity: Arity): Boolean = get == Some((name, arity))
    /** knock out the currentRec if appropriate; if it is shadowed, it won't be called */
    def shadow(name: Name): CurrentRec = CurrentRec(get.filterNot(name == _._1))
    /** knock out the currentRec if appropriate; if it is shadowed, it won't be called */
    def shadow(names: Seq[Name]): CurrentRec = CurrentRec(get.filterNot(names contains _._1))
  }
  object CurrentRec {
    def none = CurrentRec(None)
    def apply(name: Name, arity: Arity): CurrentRec = CurrentRec(Some((name,arity)))
  }

  case class RecursiveVars(get: Set[Name]) extends AnyVal {
    def contains(name: Name): Boolean = get.contains(name)
    def +(name: Name): RecursiveVars = RecursiveVars(get + name)
    def ++(names: Seq[Name]): RecursiveVars = RecursiveVars(get ++ names)
    def -(name: Name): RecursiveVars = RecursiveVars(get.filterNot(name == _))
    def --(names: Seq[Name]): RecursiveVars = RecursiveVars(get.filterNot(names contains _))
  }
  object RecursiveVars {
      def empty = RecursiveVars(Set())
  }
}

package object compilation extends TailCalls with CompileLambda with CompileLet1 with CompileLetRec with CompileLookupVar with CompileFunctionApplication with CompileIf0 {
  type D = Double
  type V = Value
  type R = Result
  type TC = TailCall.type
  type SelfTC = SelfTailCall.type
  type Arity = Int
  type IsTail = Boolean
  val IsTail = true
  val IsNotTail = false

  /**
   * The annotation contains a `Set[Name]` of free variables for the term,
   * and a `Vector[Name]` which is a stack of bound variables at the term
   * (bound variable stack is also called "the environment").
   *
   * Ex: `x -> y -> x + y`, free variables of `x + y` will be `Set(x, y)`,
   * and bound variables will be `Vector(y, x)`.
   */
  type TermC = AnnotatedTerm[Term.F, ((Set[Name], Vector[Name]), RecursiveVars)]

  def unTermC(t: TermC): Term = t.map(_._1._1)

  def env(t: TermC): Vector[Name] = t.annotation._1._2

  def freeVars(t: TermC): Set[Name] = t.annotation._1._1

  def recVars(t: TermC): RecursiveVars = t.annotation._2

  def stackSize(t: TermC): Int = stackSize(freeVars(t), env(t))

  def warnAssert(b: Boolean, s: => String) = if (!b) {
    System.err.println(s)
  }


  /**
   * Given a set of free variables, and a stack of bound variables, figure out
   * how many elements from `bound` stack we need to be able to resolve all free vars.
   *
   * Ex: Set(x,y) and bound = Vector(x,p,q,r,y,z), arity would be: 5, since we need `bound.take(5)`
   * to have access to both `x` and `y`.
   */
  def stackSize(freeVars: Set[Name], bound: Vector[Name]): Int =
    if (freeVars.isEmpty) 0
    else freeVars.view.map(fv => bound.indexOf(fv)).max + 1

  def compile(builtins: Name => Computation)(e: Term): Computation =
    compile(builtins, checkedAnnotateBound(e), CurrentRec.none, IsTail)

  def normalize(builtins: Name => Computation)(e: Term): Term = {
    val c = compile(builtins)(e)
    val r = Result()
    val x = Term.etaNormalForm(Value(c(null, r), r.boxed).decompile)
    println("!@#!@#!@# TODO")
    println(x)
    println(org.unisonweb.util.PrettyPrint.prettyTerm(x).render(40))
    Term.fullyDecompile(x, Vector.empty)
  }

  def checkedAnnotateBound(e: Term): TermC = {
    assert(e.annotation.isEmpty, s"reannotating term with free vars: ${e.annotation}\n" + Render.renderIndent(e))
    annotateRecVars(ABT.annotateBound(e))
  }

  def compile(builtins: Name => Computation, termC: TermC, currentRec: CurrentRec, isTail: IsTail): Computation = {
    // System.out.println("[debug] compiling:\n" + Render.renderIndent(termC))

    @inline def compile1(isTail: IsTail)(termC: TermC): Computation = compile(builtins, termC, currentRec, isTail)
    @inline def compile2(isTail: IsTail, currentRec: CurrentRec)(termC: TermC): Computation = compile(builtins, termC, currentRec, isTail)

    termC match {
      case Term.Num(n) => compileNum(n)
      case Term.Builtin(name) => builtins(name)
      case Term.Compiled(c) => Return(c)(unTermC(termC)) // todo: can we do a better job tracking the uncompiled form?
      case Term.Var(name) => compileVar(currentRec, name, env(termC))
      case Term.If0(cond, if0, ifNot0) =>
        val compiledCond = compile1(IsNotTail)(cond)
        val compiledIf0 = compile1(isTail)(if0)
        val compiledIfNot0 = compile1(isTail)(ifNot0)

        compileIf0(termC, compiledCond, compiledIf0, compiledIfNot0)

      case Term.Lam(names, body) =>
        // codegen Lambda1, Lambda2, ... Lambda<max-arity> will extend Lambda
        // they should handle over and under-application in the different apply methods
        // only reason we needed to pass more than just the compiled body + metadata (names, orig term)
        // is to handle under-application, because under-application would substitute away the
        // bound variables, and then recompile

        // grab all the free variables referenced by the body of the lambda (not bound by the lambda itself)
        // get them off the stack when you build the lambda
        //  (compile/get all those `Var`s)
        //  take those Values,
            // decompile them,
            // substitute them into the body of the lambda, being careful about name clashes
            // now the lambda has no more free variables; good to compile with happy path

        compileLambda(termC, names, body, currentRec, r => t => compile2(IsTail, r)(t))

      case Term.LetRec(bindings, body) =>
        val shadowCurrentRec = currentRec.shadow(bindings.map(_._1))
        val compiledBindings = compileLetRecBindings(shadowCurrentRec, bindings.toArray, compile2(IsNotTail, _))
        val compiledBody = compile2(isTail, shadowCurrentRec)(body)

        compileLetRec(termC, bindings.map(_._1).toArray zip compiledBindings, compiledBody)

      case Term.Let1(name, binding, body) =>
        val compiledBinding = compile2(IsNotTail, currentRec)(binding)
        val compiledBody = compile2(isTail, currentRec.shadow(name))(body)

        compileLet1(termC, compiledBinding, compiledBody)

      case Term.Apply(fn, args) =>
        //todo think through conditions to safely elide tailcall

        (fn, args) match {
          // Term can represent this call with no arguments, though the parser would never produce it.
          case (fn, List()) =>
            sys.error("the parser isn't supposed to produce this")

          // fully saturated call to the nearest enclosing recursive function
          // e.g. fac n = n * (fac (n-1))
          //                  ^^^^^^^^^^^
          case (Term.Var(v), args) if currentRec.contains(v, args.length) =>
            sys.error("Didn't expect this to occur anymore, because recursive calls are subst'ed for Delayed terms.")

          // if applying fully-saturated built-in
          case (Term.Builtin(name), args) if (builtins(name) match {
            case Return(l: Lambda) => l.arity == args.size
            case _ => false
          }) =>
            val compiledArgs = args.view.map(compile1(IsNotTail)).toArray
            compile1(IsNotTail)(fn) match {
              case Return(fn: Lambda) =>
                staticCall(termC, fn, compiledArgs, IsNotTail)
            }

          case _ =>
            /* Two cases to consider:
              1. static call, eg `id 42`
              2. dynamic call, eg `(f x -> f x) id 42`
                                           ^^^ is a dynamic application
              todo: may want to break out a self-call that isn't fully saturated; special case of dynamicCall
                    could conceivably be done more efficiently
             */
            val compiledArgs = args.view.map(compile1(IsNotTail)).toArray
            compile1(IsNotTail)(fn) match {
              // if cast fails, then it's a type error: applying arguments to non-Lambda value
              case Return(fn: Lambda) => staticCall(termC, fn, compiledArgs, isTail)
              case compiledDynamic => dynamicCall(termC, compiledDynamic, compiledArgs, isTail)
            }
        }
    }
  }

  def compileNum(d: Double) = {
    class CompiledNum extends Computation0(Term.Num(d)) {
      def apply(rec: Lambda, r: R) = { r.boxed = null; d }
    }
    new CompiledNum
  }

  def compileRefVar(currentRec: CurrentRec, name: Name, env: Vector[Name]): Computation = {
    if (currentRec.contains(name))
      compileCurrentRec(name)
    else
      env.indexOf(name) match {
        case -1 => sys.error("unknown variable: " + name)
        case i => compileLookupRef(i, Term.Var(name))
      }
  }

  def compileVar(currentRec: CurrentRec, name: Name, env: Vector[Name]): Computation = {
    if (currentRec.contains(name))
      compileCurrentRec(name)
    else
      env.indexOf(name) match {
        case -1 => sys.error("unknown variable: " + name)
        case i => compileLookupVar(i, Term.Var(name))
      }
  }

  def compileCurrentRec(name: Name) = {
    class GetCurrentRec extends Computation0(Term.Var(name)) {
      def apply(rec: Lambda, r: R) = { r.boxed = rec; 0.0 }
    }
    new GetCurrentRec
  }

  def compileDelayed(f: Lazy[Value])(term: Term) = {
    def decompiled: Term = if (f.evaluated) Term.Compiled(f.value) else term

    class LazyReturn extends Computation0(decompiled) {
      def apply(rec: Lambda, r: R) = f.value(r)
    }
    new LazyReturn
  }

  def annotateRecVars[A](term: AnnotatedTerm[Term.F, A]) =
    term.annotateDown[(Boolean, RecursiveVars), (A, RecursiveVars)](false -> RecursiveVars.empty) {
      case (s @ (collecting, rv), AnnotatedTerm(a, abt)) =>
        import ABT._
        val a2 = a -> rv
        abt match {
          case Var_(name) => s -> a2
          case Abs_(name, body) =>
            if (collecting)
              (collecting, rv + name) -> a2 // introduce a rec var
            else
              (collecting, rv - name) -> a2 // shadow a rec var
          case Tm_(f) =>
            import Term.F._
            f match {
              case Rec_(_) => (true, rv) -> a2 // start collecting
              case LetRec_(_, _) => (false, rv) -> a2 // stop collecting
              case _ => s -> a2
            }
        }
    }

  def hasTailRecursiveCall(currentRec: CurrentRec, body: TermC): Boolean = !currentRec.isEmpty && {
    import ABT._
    body.get match {
      case Var_(name) => false
      case Abs_(name, body) => hasTailRecursiveCall(currentRec.shadow(name), body)
      case Tm_(f) =>
        import Term.F._
        def check(body: TermC): Boolean = hasTailRecursiveCall(currentRec, body)
        f match {
          case Apply_(AnnotatedTerm(_, Var_(name)), args) if currentRec.contains(name, args.size) => true
          case Apply_(AnnotatedTerm(_, Tm_(Delayed_(name, _))), args) if currentRec.contains(name, args.size) => true
          case Apply_(_, _) => false
          case Lam_(body) => check(body)
          case LetRec_(_, body) => check(body)
          case Let_(_, body) => check(body)
          case Rec_(body) => check(body)
          case If0_(_, ifZero, ifNotZero) => check(ifZero) || check(ifNotZero)
          case Builtin_(_) | Num_(_) => false
          case Delayed_(_, _) | Compiled_(_) => false
          case Yield_(_) | Handle_(_, _) => ???
        }
    }
  }

  def render(d: D, v: V): String = { if (v eq null) d.toString else "(" + Render1.render(v.decompile) + ")" }
  def render(slot: Slot): String = render(slot.unboxed, slot.boxed)

  import scala.annotation.elidable
  @elidable(elidable.FINE) // doesn't seem to do anything, even with -Xelide-below ALL
  def logFine(s: String): Unit = ()//println(s)

  def evaluate(rt: Computation, r: R): D =
    try rt(null, r) catch { case e: TC => loop(r) }

}
