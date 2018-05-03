package org.unisonweb

import util.{GraphCodec, Sink, Source}
import Term.Term
import Term.F._

import scala.annotation.switch

object Codecs {

  sealed trait Node

  object Node {
    case class Term(get: org.unisonweb.Term.Term) extends Node
    case class Param(get: org.unisonweb.Param) extends Node
  }

  implicit val nodeGraphCodec: GraphCodec[Node,Ref] = new GraphCodec[Node,Ref] {

    def dereference(r: Ref) = Node.Param(r.value)
    def isReference(node: Node) = node match {
      case Node.Param(r) => r.isRef
      case _ => false
    }
    def inject(r: Ref): Node = Node.Param(r)

    def stageDecoder(src: Source): () => Node = GraphCodec.decoder(src, inject) {
      new GraphCodec.Decoder[Node,Ref] {
        val stackU = new Array[U](512)
        val stackB = new Array[B](512)
        val R = compilation.Result()
        def makeReference(position: Long, prefix: Array[Byte]) =
          // todo - parse Name out of prefix
          new Ref("v"+position.toString, null)
        def setReference(r: Ref, v: Node): Unit = {
          r.value = v match {
            case Node.Param(v) => v.toValue
            case Node.Term(_) => sys.error("cannot set a reference to a term")
          }
        }
        def decode(readChildOption: () => Option[Node]): Node = {
          def readChildTerm(): Term = readChildTermOption().get
          def readChildTermOption(): Option[Term] = readChildOption() match {
            case Some(Node.Term(t)) => Some(t)
            case _ => None
          }
          def readChildParam(): Param = readChildParamOption().get
          def readChildParamOption(): Option[Param] = readChildOption() match {
            case Some(Node.Param(p)) => Some(p)
            case _ => None
          }
          def readChildValueOption(): Option[Value] = readChildOption() match {
            case Some(Node.Param(p)) => Some(p.toValue)
            case _ => None
          }
          def readChildValue(): Value = readChildValueOption().get
          val tag = src.getByte
          if (tag <= 20) Node.Term { (tag: @switch) match {
            case 0  => ABT.Var(src.getString)
            case 1  => ABT.Abs(src.getString, readChildTerm())
            case 2  => Term.Id(readId(src))
            case 3  => Term.Constructor(readId(src), readConstructorId(src))
            case 4  => Term.Request(readId(src), readConstructorId(src))
            case 5  => Term.Text(src.getText)
            case 6  => Term.Unboxed(src.getLong, readUnboxedType(src))
            case 7  => Term.Sequence(readSequence(readChildTermOption _))
            case 8  => ABT.Tm(Lam_(readChildTerm()))
            case 9  => Term.Apply(readChildTerm(), readList(readChildTermOption _):_*)
            case 10 => ABT.Tm(Rec_(readChildTerm()))
            case 11 => ABT.Tm(Let_(readChildTerm(), readChildTerm()))
            case 12 => Term.If(readChildTerm(), readChildTerm(), readChildTerm())
            case 13 => Term.And(readChildTerm(), readChildTerm())
            case 14 => Term.Or(readChildTerm(), readChildTerm())
            case 15 => /* Match */
              val patternsish = Source.getFramedArray(src) {
                src => (readPattern(src), src.getBoolean)
              }
              val scrutinee = readChildTerm()
              val cases = patternsish map { case (pat, hasGuard) =>
                Term.MatchCase(
                  pat,
                  if (hasGuard) Some(readChildTerm()) else None,
                  readChildTerm()
                )
              }
              Term.Match(scrutinee)(cases: _*)
            case 16 => Term.Handle(readChildTerm())(readChildTerm())
            case 17 => Term.EffectPure(readChildTerm())
            case 18 =>
              val id = readId(src)
              val cid = readConstructorId(src)
              val children = readList(readChildTermOption _) // todo: slow
              Term.EffectBind(id,cid,children.init,children.last)
            case 19 =>
              val children = readList(readChildTermOption _) // todo: slow
              ABT.Tm(LetRec_(children.init, children.last))
            case 20 =>
              val name = src.getString
              val param = readChildParam() // note, this changes src
              Term.Compiled(param, name)
        }}
        else Node.Param { (tag: @switch) match {
          case 21 => Value.Unboxed(src.getLong, readUnboxedType(src))
          case 22 => /* Lambda */
            // in order to do compilation we need the compilation environment
            val c = compilation.compileTop(Environment.standard)(readChildTerm())
            // in order to do evaluation we need a runtime environment / stack
            ???
          case 23 => Value.Data(readId(src),
                                readConstructorId(src),
                                readArray(readChildValueOption _))
          case 24 => Value.EffectPure(src.getLong, readChildValue())
          case 25 => /* EffectBind */
            val id = readId(src)
            val cid = readConstructorId(src)
            val children = readArray(readChildValueOption _)
            Value.EffectBind(readId(src),
                             readConstructorId(src),
                             children.init, children.last.asInstanceOf[Value.Lambda])
          case 26 => new Ref(src.getString, readChildValue())
          case 27 => /* External */
            // in order to do compilation we need the compilation environment
            val c = compilation.compileTop(Environment.standard)(readChildTerm())
            val sp0 = compilation.StackPtr.empty
            Value(compilation.evalClosed(c,R,sp0,stackU,stackB), R.boxed)
          case 28 => UnboxedType.Boolean
          case 29 => UnboxedType.Int64
          case 30 => UnboxedType.UInt64
          case 31 => UnboxedType.Float
          case t =>
            sys.error(s"unexpected tag byte $t during decoding")
        }}
      }}
    }

    def foreach(node: Node)(f: Node => Unit): Unit = node match {
      case Node.Param(r) => foreachParam(r)(f)
      case Node.Term(t) => foreachTerm(t)(f)
    }

    def foreachTerm(t: Term)(f: Node => Unit): Unit = t.get match {
      case ABT.Tm_(tm) => tm match {
        case Compiled_(p,name) => foreachParam(p)(f)
        case tm => tm foreachChild (term => f(Node.Term(term)))
      }
      case ABT.Abs_(_,body) => f(Node.Term(body))
      case ABT.Var_(_) => ()
    }

    def foreachParam(p: Param)(f: Node => Unit): Unit = p match {
      case lam : Value.Lambda => foreachTerm(lam.decompile)(f)
      case e : Builtins.External => foreachTerm(e.decompile)(f)
      case p => p foreachChild (p => f(Node.Param(p)))
    }

    def writeBytePrefix(node: Node, sink: Sink): Unit = node match {
      case Node.Term(t) => writeTermBytePrefix(t, sink)
      case Node.Param(p) => writeParamBytePrefix(p, sink)
    }


    def writeTermBytePrefix(t: Term, sink: Sink): Unit = t.get match {
      case ABT.Var_(n)                  => sink putByte 0
        sink putString n.toString

      case ABT.Abs_(n,_)                => sink putByte 1
        sink putString n.toString

      case ABT.Tm_(f) => f match {
        case Id_(id)                    => sink putByte 2
          writeId(id, sink)

        case Constructor_(id,cid)       => sink putByte 3
          writeId(id, sink)
          writeConstructorId(cid, sink)

        case Request_(id,cid)           => sink putByte 4
          writeId(id, sink)
          writeConstructorId(cid, sink)

        case Text_(txt)                 => sink putByte 5
          sink putText txt

        case Unboxed_(u, t)             => sink putByte 6
          sink putLong u
          writeUnboxedType(t, sink)

        case Sequence_(_)               => sink putByte 7
        case Lam_(_)                    => sink putByte 8
        case Apply_(_,_)                => sink putByte 9
        case Rec_(_)                    => sink putByte 10
        case Let_(_,_)                  => sink putByte 11
        case If_(_,_,_)                 => sink putByte 12
        case And_(_,_)                  => sink putByte 13
        case Or_(_,_)                   => sink putByte 14
        case Match_(_,cases)            => sink putByte 15
          sink.putFramedSeq(cases) {
            (sink, c) =>
              writePattern(c.pattern, sink)
              sink putBoolean c.guard.isEmpty
          }

        case Handle_(_,_)               => sink putByte 16
        case EffectPure_(_)             => sink putByte 17
        case EffectBind_(id,cid,args,_) => sink putByte 18
          writeId(id, sink)
          writeConstructorId(cid, sink)

        case LetRec_(_,_)               => sink putByte 19
        case Compiled_(value,name)      => sink putByte 20
          sink putString name.toString
          writeParamBytePrefix(value, sink)
      }
    }

    def writeParamBytePrefix(graph: Param, sink: Sink): Unit = graph match {
      case Value.Unboxed(u, typ)           => sink putByte 21
        sink.putLong(u)
        writeUnboxedType(typ, sink)

      case l: Value.Lambda                 => sink putByte 22
        writeTermBytePrefix(l.decompile,
                            sink)

      case Value.Data(id, cid, _)          => sink putByte 23
        writeId(id, sink)
        writeConstructorId(cid, sink)

      case Value.EffectPure(u, _)          => sink putByte 24
        sink putLong u

      case Value.EffectBind(id, cid, _, _) => sink putByte 25
        writeId(id, sink)
        writeConstructorId(cid, sink)

      case r: Ref                          => sink putByte 26
        sink putString r.name.toString

      case e: Builtins.External            => sink putByte 27

      case UnboxedType.Boolean             => sink putByte 28
      case UnboxedType.Int64               => sink putByte 29
      case UnboxedType.UInt64              => sink putByte 30
      case UnboxedType.Float               => sink putByte 31
      case t =>
        sys.error(s"unexpected Param type ${t.getClass} in writeParamBytePrefix")
    }

    final def readId(source: Source): Id = (source.getByte: @switch) match {
      case 0 => Id.Builtin(source.getString)
      case 1 => Id.HashRef(Hash(source.getFramed))
    }

    final def writeId(id: Id, sink: Sink): Unit = id match {
      case Id.Builtin(name) =>
        sink putByte 0
        sink putString name.toString
      case Id.HashRef(h) =>
        sink putByte 1
        sink putFramed h.bytes
    }

    final def readConstructorId(source: Source): ConstructorId =
      ConstructorId(source.getInt)

    final def writeConstructorId(cid: ConstructorId, sink: Sink): Unit =
      sink putInt cid.toInt

    final def readUnboxedType(source: Source): UnboxedType =
      (source.getByte: @switch) match {
        case 0 => UnboxedType.Boolean
        case 1 => UnboxedType.Int64
        case 2 => UnboxedType.UInt64
        case 3 => UnboxedType.Float
      }

    final def writeUnboxedType(t: UnboxedType, sink: Sink): Unit = t match {
      case UnboxedType.Boolean => sink.putByte(0)
      case UnboxedType.Int64 => sink.putByte(1)
      case UnboxedType.UInt64 => sink.putByte(2)
      case UnboxedType.Float => sink.putByte(3)
    }

    final def readPattern(src: Source): Pattern = (src.getByte: @switch) match {
      case 0 => Pattern.LiteralU(src.getLong, readUnboxedType(src))
      case 1 => Pattern.Wildcard
      case 2 => Pattern.Uncaptured
      case 3 => Pattern.Data(readId(src), readConstructorId(src),
                     Source.getFramedList(src)(readPattern))
      case 4 => Pattern.As(readPattern(src))
      case 5 => Pattern.EffectPure(readPattern(src))
      case 6 => Pattern.EffectBind(readId(src), readConstructorId(src),
                           Source.getFramedList(src)(readPattern),
                           readPattern(src))
    }

    final def writePattern(p: Pattern, sink: Sink): Unit = p match {
      case Pattern.LiteralU(u, typ) =>
        sink.putByte(0)
        sink.putLong(u)
        writeUnboxedType(typ, sink)
      case Pattern.Wildcard =>
        sink.putByte(1)
      case Pattern.Uncaptured =>
        sink.putByte(2)
      case Pattern.Data(id,cid,patterns) =>
        sink.putByte(3)
        writeId(id, sink)
        writeConstructorId(cid, sink)
        sink.putFramedSeq(patterns)((sink,p) => writePattern(p,sink))
      case Pattern.As(p) =>
        sink.putByte(4)
        writePattern(p, sink)
      case Pattern.EffectPure(p) =>
        sink.putByte(5)
        writePattern(p, sink)
      case Pattern.EffectBind(id,cid,patterns,continuation) =>
        sink.putByte(6)
        writeId(id, sink)
        writeConstructorId(cid, sink)
        sink.putFramedSeq(patterns)((sink,p) => writePattern(p,sink))
        writePattern(continuation, sink)
    }

    final def readSequence[A](readChild: () => Option[A]): util.Sequence[A] = {
      @annotation.tailrec
      def loop(s: util.Sequence[A]): util.Sequence[A] =
        readChild() match {
          case Some(g) => loop(s :+ g)
          case None => s
        }
      loop(util.Sequence.empty)
    }

    final def readArray[A:reflect.ClassTag](readChild: () => Option[A]): Array[A] = {
      val buf = new collection.mutable.ArrayBuffer[A]
      @annotation.tailrec
      def loop: Array[A] = readChild() match {
        case Some(g) => buf += g; loop
        case None => buf.toArray
      }
      loop
    }

    final def readList[A](readChild: () => Option[A]): List[A] = {
      @annotation.tailrec
      def loop(s: List[A]): List[A] =
        readChild() match {
          case Some(g) => loop(g :: s)
          case None => s.reverse
        }
      loop(Nil)
    }
  }

  //implicit val termGraphCodec: GraphCodec[Term,Nothing] = {
  //  new GraphCodec[Term,Nothing] {
  //    type G = Term
  //    type R = Nothing
  //    import Term.F._

  //    def inject(r: Nothing): Term = sys.error("unpossible")

  //    def writeBytePrefix(graph: G, sink: Sink): Unit = ???

  //    def foreach(graph: G)(f: G => Unit): Unit = graph.get match {
  //      // case ABT.Tm_(Compiled()) => tm foreachChild f
  //      case ABT.Tm_(tm) => tm foreachChild f
  //      case ABT.Abs_(_,body) => foreach(body)(f)
  //      case ABT.Var_(_) => ()
  //    }

  //    }

  //    def makeReference(position: Long, prefix: Array[Byte]): R = sys.error("unpossible")
  //    def setReference(ref: R, referent: G): Unit = sys.error("unpossible")
  //    def isReference(graph: G): Boolean = false
  //    def dereference(graph: R): G = sys.error("unpossible")

  //  }
  //}

}
