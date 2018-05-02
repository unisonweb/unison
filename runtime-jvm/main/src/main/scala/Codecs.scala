package org.unisonweb

import util.{GraphCodec, Sink, Source}
import Term.Term

import scala.annotation.switch

object Codecs {
  // make sure valueGraphCodec doesn't eagerly call termGraphCodec :)
  implicit val valueGraphCodec: GraphCodec[Param,Ref] = ???

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

  def writeUnboxedType(t: UnboxedType, sink: Sink): Unit = ???

  def writePattern(p: Pattern, sink: Sink): Unit = ???


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
            writeUnboxedType(t, sink)
            sink putLong u
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
            val len = cases.length
            sink putInt len
            def bool(b: Boolean): Byte = if (b) 1:Byte else 0:Byte
            cases foreach { c =>
              writePattern(c.pattern, sink)
              sink putByte (bool(c.guard.isEmpty))
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

      def readId(source: Source): Id = ???
      def readConstructorId(source: Source): ConstructorId = ???
      def readSequence(readChild: () => Option[G]): util.Sequence[G] = ???
      def readUnboxedType(source: Source): UnboxedType = ???
      def readList(readChild: () => Option[G]): List[G] = ???


      def stageDecoder(src: Source): () => Term = {
        val valueDecoder = valueGraphCodec.stageDecoder(src)
        GraphCodec.decoder(src) {
          new GraphCodec.Decoder[G, Nothing] {

            def makeReference(position: Long, prefix: Array[Byte]): R =
              sys.error("unpossible")

            def setReference(ref: R, referent: G): Unit =
              sys.error("unpossible")

            def isReference(graph: G): Boolean = false

            def decode(readChild: () => Option[G]): G = {
              (src.getByte: @switch) match {
                case VarMarker =>
                  ABT.Var(src.getString)
                case AbsMarker =>
                  ABT.Abs(
                    src.getString,
                    readChild().get
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
                  val typ = readUnboxedType(src)
                  Term.Unboxed(src.getLong, typ)
                case SequenceMarker =>
                  Term.Sequence(readSequence(readChild))
                case LamMarker =>
                  ABT.Tm(Lam_(readChild().get))
                case ApplyMarker =>
                  Term.Apply(readChild().get, readList(readChild):_*)
                case RecMarker =>
                  ABT.Tm(Rec_(readChild().get))
                case LetMarker =>
                  ABT.Tm(Let_(readChild().get, readChild().get))
                case IfMarker =>
                  Term.If(readChild().get, readChild().get, readChild().get)
                case AndMarker =>
                  Term.And(readChild().get, readChild().get)
                case OrMarker =>
                  Term.Or(readChild().get, readChild().get)
                case MatchMarker => ???
                case CompiledMarker =>
                  val name = src.getString
                  val param = valueDecoder() // note, this changes src
                  Term.Compiled(param, name)
                case RequestMarker =>
                  Term.Request(readId(src), readConstructorId(src))
                case HandleMarker =>
                  Term.Handle(readChild().get)(readChild().get)
                case EffectPureMarker =>
                  Term.EffectPure(readChild().get)
                case EffectBindMarker =>
                  val id = readId(src)
                  val cid = readConstructorId(src)
                  val children = readList(readChild) // todo: slow
                  Term.EffectBind(id,cid,children.init,children.last)
                case LetRecMarker =>
                  val children = readList(readChild) // todo: slow
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
