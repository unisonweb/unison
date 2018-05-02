package org.unisonweb

import util.{GraphCodec, Sink, Source}
import Term.Term

import scala.annotation.switch

object Codecs {
  // make sure valueGraphCodec doesn't eagerly call termGraphCodec :)
  implicit val valueGraphCodec: GraphCodec[Param,Ref] = new GraphCodec[Param,Ref] {
    def writeBytePrefix(graph: Param, sink: Sink): Unit = ???
    def bytePrefixIndex(graph: Param, index: Int): Byte = ???
    def bytePrefixLength(graph: Param): Int = ???

    def dereference(graph: Ref): Param = graph.value
    def foreach(graph: Param)(f: Param => Unit): Unit = graph foreachChild f
    def isReference(graph: Param): Boolean = graph.isRef

    def stageDecoder(src: Source): () => Param = ???
  }

  def writeId(id: Id, sink: Sink): Unit = id match {
    case Id.Builtin(name) =>
      sink putByte 0
      sink putString name.toString
    case Id.HashRef(h) =>
      sink putByte 1
      sink putFramed h.bytes
  }

  def writeConstructorId(cid: ConstructorId, sink: Sink): Unit =
    sink putInt cid.toInt

  def writeUnboxedType(t: UnboxedType, sink: Sink): Unit = t match {
    case UnboxedType.Boolean => sink.putByte(0)
    case UnboxedType.Int64 => sink.putByte(1)
    case UnboxedType.UInt64 => sink.putByte(2)
    case UnboxedType.Float => sink.putByte(3)
  }

  def writePattern(p: Pattern, sink: Sink): Unit = p match {
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


  implicit val termGraphCodec: GraphCodec[Term,Nothing] = {
    new GraphCodec[Term,Nothing] {
      type G = Term
      type R = Nothing
      import Term.F._

      def writeBytePrefix(graph: G, sink: Sink): Unit = graph.get match {
        case ABT.Var_(n) =>
          sink putByte VarMarker.toByte
          sink putString n.toString
        case ABT.Abs_(n,_) =>
          sink putByte AbsMarker.toByte
          sink putString n.toString
        case ABT.Tm_(f) => f match {
          case Constructor_(id,cid) =>
            sink putByte ConstructorMarker.toByte
            writeId(id, sink)
            writeConstructorId(cid, sink)
          case Id_(id) =>
            sink putByte IdMarker.toByte
            writeId(id, sink)
          case Text_(txt) =>
            sink putByte TextMarker.toByte
            sink putText txt
          case Unboxed_(u, t) =>
            sink putByte UnboxedMarker.toByte
            sink putLong u
            writeUnboxedType(t, sink)
          case Sequence_(_) => sink putByte SequenceMarker.toByte
          case Lam_(_) => sink putByte LamMarker.toByte
          case Apply_(_,_) => sink putByte ApplyMarker.toByte
          case Rec_(_) => sink putByte RecMarker.toByte
          case Let_(_,_) => sink putByte LetMarker.toByte
          case If_(_,_,_) => sink putByte IfMarker.toByte
          case And_(_,_) => sink putByte AndMarker.toByte
          case Or_(_,_) => sink putByte OrMarker.toByte
          case Match_(_,cases) =>
            sink putByte MatchMarker.toByte
            sink.putFramedSeq(cases) {
              (sink, c) =>
                writePattern(c.pattern, sink)
                sink putBoolean c.guard.isEmpty
            }
          case Compiled_(value,name) =>
            sink putByte CompiledMarker.toByte
            sink putString name.toString
            valueGraphCodec.writeBytePrefix(value,sink)
          case Request_(id,cid) =>
            sink putByte RequestMarker.toByte
            writeId(id, sink)
            writeConstructorId(cid, sink)
          case Handle_(_,_) => sink putByte HandleMarker.toByte
          case EffectPure_(_) => sink putByte EffectPureMarker.toByte
          case EffectBind_(id,cid,args,_) =>
            sink putByte EffectPureMarker.toByte
            writeId(id, sink)
            writeConstructorId(cid, sink)
          case LetRec_(_,_) =>
            sink putByte LetRecMarker.toByte
        }
      }

      def bytePrefixLength(graph: G): Int = {
        val bb = java.nio.ByteBuffer.allocate(32)
        val sink: Sink = Sink.fromByteBuffer(bb, bb => {})
        writeBytePrefix(graph, sink)
        sink.position.toInt
      }

      def bytePrefixIndex(graph: G, index: Int): Byte =
        bytePrefix(graph)(index.toLong)

      def foreach(graph: G)(f: G => Unit): Unit = graph.get match {
        case ABT.Tm_(tm) => tm foreachChild f
        case ABT.Abs_(_,body) => foreach(body)(f)
        case ABT.Var_(_) => ()
      }

      def readId(source: Source): Id =
        (source.getByte: @switch) match {
          case 0 => Id.Builtin(source.getString)
          case 1 => Id.HashRef(Hash(source.getFramed))
        }

      def readConstructorId(source: Source): ConstructorId =
        ConstructorId(source.getInt)

      def readUnboxedType(source: Source): UnboxedType =
        (source.getByte: @switch) match {
          case 0 => UnboxedType.Boolean
          case 1 => UnboxedType.Int64
          case 2 => UnboxedType.UInt64
          case 3 => UnboxedType.Float
        }

      def readPattern(src: Source): Pattern =
        (src.getByte: @switch) match {
          case 0 =>
            Pattern.LiteralU(src.getLong, readUnboxedType(src))
          case 1 =>
            Pattern.Wildcard
          case 2 =>
            Pattern.Uncaptured
          case 3 =>
            Pattern.Data(readId(src), readConstructorId(src),
                         Source.getFramedList(src)(readPattern))
          case 4 =>
            Pattern.As(readPattern(src))
          case 5 =>
            Pattern.EffectPure(readPattern(src))
          case 6 =>
            Pattern.EffectBind(readId(src), readConstructorId(src),
                               Source.getFramedList(src)(readPattern),
                               readPattern(src))
        }

      def readSequence(readChild: () => Option[G]): util.Sequence[G] = {
        @annotation.tailrec
        def loop(s: util.Sequence[G]): util.Sequence[G] =
          readChild() match {
            case Some(g) => loop(s :+ g)
            case None => s
          }
        loop(util.Sequence.empty)
      }

      def readList(readChild: () => Option[G]): List[G] = {
        @annotation.tailrec
        def loop(s: List[G]): List[G] =
          readChild() match {
            case Some(g) => loop(g :: s)
            case None => s.reverse
          }
        loop(Nil)
      }


      def stageDecoder(src: Source): () => Term = {
        val valueDecoder = valueGraphCodec.stageDecoder(src)
        GraphCodec.decoder(src) {
          new GraphCodec.Decoder[G, Nothing] {

            def makeReference(position: Long, prefix: Array[Byte]): R =
              sys.error("unpossible")

            def setReference(ref: R, referent: G): Unit =
              sys.error("unpossible")

            def isReference(graph: G): Boolean = false

            def decode(readChildOption: () => Option[G]): G = {
              def readChild(): G = readChildOption().get
              (src.getByte: @switch) match {
                case VarMarker =>
                  ABT.Var(src.getString)
                case AbsMarker =>
                  ABT.Abs(
                    src.getString,
                    readChild()
                  )
                case ConstructorMarker =>
                  Term.Constructor(
                    readId(src),
                    readConstructorId(src)
                  )
                case IdMarker =>
                  Term.Id(readId(src))
                case TextMarker =>
                  Term.Text(src.getText)
                case UnboxedMarker =>
                  Term.Unboxed(src.getLong, readUnboxedType(src))
                case SequenceMarker =>
                  Term.Sequence(readSequence(readChildOption))
                case LamMarker =>
                  ABT.Tm(Lam_(readChild()))
                case ApplyMarker =>
                  Term.Apply(readChild(), readList(readChildOption):_*)
                case RecMarker =>
                  ABT.Tm(Rec_(readChild()))
                case LetMarker =>
                  ABT.Tm(Let_(readChild(), readChild()))
                case IfMarker =>
                  Term.If(readChild(), readChild(), readChild())
                case AndMarker =>
                  Term.And(readChild(), readChild())
                case OrMarker =>
                  Term.Or(readChild(), readChild())
                case MatchMarker =>
                  val patternsish = Source.getFramedArray(src) {
                    src => (readPattern(src), src.getBoolean)
                  }
                  val scrutinee = readChild()
                  val cases = patternsish map { case (pat, hasGuard) =>
                    Term.MatchCase(
                      pat,
                      if (hasGuard) Some(readChild()) else None,
                      readChild()
                    )
                  }
                  Term.Match(scrutinee)(cases: _*)
                case CompiledMarker =>
                  val name = src.getString
                  val param = valueDecoder() // note, this changes src
                  Term.Compiled(param, name)
                case RequestMarker =>
                  Term.Request(readId(src), readConstructorId(src))
                case HandleMarker =>
                  Term.Handle(readChild())(readChild())
                case EffectPureMarker =>
                  Term.EffectPure(readChild())
                case EffectBindMarker =>
                  val id = readId(src)
                  val cid = readConstructorId(src)
                  val children = readList(readChildOption) // todo: slow
                  Term.EffectBind(id,cid,children.init,children.last)
                case LetRecMarker =>
                  val children = readList(readChildOption) // todo: slow
                  ABT.Tm(LetRec_(children.init, children.last))
              }
            }
          }
        }
      }

      def makeReference(position: Long, prefix: Array[Byte]): R = sys.error("unpossible")
      def setReference(ref: R, referent: G): Unit = sys.error("unpossible")
      def isReference(graph: G): Boolean = false
      def dereference(graph: R): G = sys.error("unpossible")

      final val AbsMarker = 1
      final val AndMarker = 12
      final val ApplyMarker = 5
      final val CompiledMarker = 15
      final val ConstructorMarker = 4
      final val EffectBindMarker = 19
      final val EffectPureMarker = 18
      final val HandleMarker = 17
      final val IdMarker = 3
      final val IfMarker = 11
      final val LamMarker = 2
      final val LetMarker = 10
      final val LetRecMarker = 20
      final val MatchMarker = 14
      final val OrMarker = 13
      final val RecMarker = 9
      final val RequestMarker = 16
      final val SequenceMarker = 8
      final val TextMarker = 7
      final val UnboxedMarker = 6
      final val VarMarker = 0
    }

  }
}
