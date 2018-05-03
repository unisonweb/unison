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
    def inject(r: Ref) = Node.Param(r)

    def stageDecoder(src: Source): () => Node = GraphCodec.decoder(src, inject) {
      new GraphCodec.Decoder[Node,Ref] {
        def makeReference(position: Long, prefix: Array[Byte]) =
          // todo - parse Name out of prefix
          new Ref("v"+position.toString, null)
        def setReference(r: Ref, v: Node): Unit = {
          r.value = v match { case Node.Param(v) => v.toValue }
        }
      }
    }

  //    def stageDecoder(src: Source): () => Term = {
  //      val valueDecoder = valueGraphCodec.stageDecoder(src)
  //      GraphCodec.decoder(src) {
  //        new GraphCodec.Decoder[G, Nothing] {

  //          def makeReference(position: Long, prefix: Array[Byte]): R =
  //            sys.error("unpossible")

  //          def setReference(ref: R, referent: G): Unit =
  //            sys.error("unpossible")

  //          def isReference(graph: G): Boolean = false

  //          def decode(readChildOption: () => Option[G]): G = {
  //            def readChild(): G = readChildOption().get
  //            (src.getByte: @switch) match {
  //              case VarTag =>
  //                ABT.Var(src.getString)
  //              case AbsTag =>
  //                ABT.Abs(
  //                  src.getString,
  //                  readChild()
  //                )
  //              case ConstructorTag =>
  //                Term.Constructor(
  //                  readId(src),
  //                  readConstructorId(src)
  //                )
  //              case IdTag =>
  //                Term.Id(readId(src))
  //              case TextTag =>
  //                Term.Text(src.getText)
  //              case UnboxedTag =>
  //                Term.Unboxed(src.getLong, readUnboxedType(src))
  //              case SequenceTag =>
  //                Term.Sequence(readSequence(readChildOption))
  //              case LamTag =>
  //                ABT.Tm(Lam_(readChild()))
  //              case ApplyTag =>
  //                Term.Apply(readChild(), readList(readChildOption):_*)
  //              case RecTag =>
  //                ABT.Tm(Rec_(readChild()))
  //              case LetTag =>
  //                ABT.Tm(Let_(readChild(), readChild()))
  //              case IfTag =>
  //                Term.If(readChild(), readChild(), readChild())
  //              case AndTag =>
  //                Term.And(readChild(), readChild())
  //              case OrTag =>
  //                Term.Or(readChild(), readChild())
  //              case MatchTag =>
  //                val patternsish = Source.getFramedArray(src) {
  //                  src => (readPattern(src), src.getBoolean)
  //                }
  //                val scrutinee = readChild()
  //                val cases = patternsish map { case (pat, hasGuard) =>
  //                  Term.MatchCase(
  //                    pat,
  //                    if (hasGuard) Some(readChild()) else None,
  //                    readChild()
  //                  )
  //                }
  //                Term.Match(scrutinee)(cases: _*)
  //              case CompiledTag =>
  //                val name = src.getString
  //                val param = valueDecoder() // note, this changes src
  //                Term.Compiled(param, name)
  //              case RequestTag =>
  //                Term.Request(readId(src), readConstructorId(src))
  //              case HandleTag =>
  //                Term.Handle(readChild())(readChild())
  //              case EffectPureTag =>
  //                Term.EffectPure(readChild())
  //              case EffectBindTagTerm=>
  //                val id = readId(src)
  //                val cid = readConstructorId(src)
  //                val children = readList(readChildOption) // todo: slow
  //                Term.EffectBind(id,cid,children.init,children.last)
  //              case LetRecTagTerm=>
  //                val children = readList(readChildOption) // todo: slow
  //                ABT.Tm(LetRec_(children.init, children.last))
  //            }
  //          }
  //        }
  //      }
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
        writeParamBytePrefix(r.value, sink)

      case e: Builtins.External            => sink putByte 27
        writeTermBytePrefix(e.decompile,
                            sink)

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

    final def readSequence(readChild: () => Option[Node]): util.Sequence[Node] = {
      @annotation.tailrec
      def loop(s: util.Sequence[Node]): util.Sequence[Node] =
        readChild() match {
          case Some(g) => loop(s :+ g)
          case None => s
        }
      loop(util.Sequence.empty)
    }

    final def readList(readChild: () => Option[Node]): List[Node] = {
      @annotation.tailrec
      def loop(s: List[Node]): List[Node] =
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
