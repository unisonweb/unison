package org.unisonweb

import util.Codecs.Sink
import util.{GraphCodec, Sequence}
import Term.Term

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

  implicit val termGraphCodec: GraphCodec[Term,Nothing] =
    new GraphCodec[Term,Nothing] {
      type G = Term
      type R = Nothing
      import Term.F._

      def writeBytePrefix(graph: G, sink: Sink): Unit = graph.get match {
        case ABT.Var_(n) =>
          sink putByte VarMarker
          sink putString n.toString
        case ABT.Abs_(n,_) =>
          sink putByte AbsMarker
          sink putString n.toString
        case ABT.Tm_(f) => f match {
          case Constructor_(id,cid) =>
            sink putByte ConstructorMarker
            writeId(id, sink)
            writeConstructorId(cid, sink)
          case Id_(id) =>
            sink putByte IdMarker
            writeId(id, sink)
          case Text_(txt) =>
            sink putByte TextMarker
            sink putText txt
          case Unboxed_(u, t) =>
            sink putByte UnboxedMarker
            writeUnboxedType(t, sink)
            sink putLong u
          case Sequence_(_) => sink putByte SequenceMarker
          case Lam_(_) => sink putByte LamMarker
          case Apply_(_,_) => sink putByte ApplyMarker
          case Rec_(_) => sink putByte RecMarker
          case Let_(_,_) => sink putByte LetMarker
          case If_(_,_,_) => sink putByte IfMarker
          case And_(_,_) => sink putByte AndMarker
          case Or_(_,_) => sink putByte OrMarker
          case Match_(_,cases) =>
            sink putByte MatchMarker
            val len = cases.length
            sink putInt len
            def bool(b: Boolean): Byte = if (b) 1:Byte else 0:Byte
            cases foreach { c =>
              writePattern(c.pattern, sink)
              sink putByte (bool(c.guard.isEmpty))
            }
          case Compiled_(value,name) =>
            sink putByte CompiledMarker
            sink putString name.toString
            valueGraphCodec.writeBytePrefix(value,sink)
          case Request_(_,_) => sink putByte CompiledMarker
          case Handle_(_,_) => sink putByte HandleMarker
          case EffectPure_(_) => sink putByte EffectPureMarker
          case EffectBind_(id,cid,args,_) =>
            sink putByte EffectPureMarker
            writeId(id, sink)
            writeConstructorId(cid, sink)
            sink putInt args.length
          case LetRec_(_,_) =>
            sink putByte LetRecMarker
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

      def nest(prefix: Array[Byte], children: Sequence[G]): G = ???

      def makeReference(position: Long, prefix: Array[Byte]): R = sys.error("unpossible")
      def setReference(ref: R, referent: G): Unit = sys.error("unpossible")
      def isReference(graph: G): Boolean = false
      def dereference(graph: R): G = sys.error("unpossible")

      val AbsMarker: Byte = 1
      val AndMarker: Byte = 12
      val ApplyMarker: Byte = 5
      val CompiledMarker: Byte = 15
      val ConstructorMarker: Byte = 4
      val EffectBindMarker: Byte = 19
      val EffectPureMarker: Byte = 18
      val HandleMarker: Byte = 17
      val IdMarker: Byte = 3
      val IfMarker: Byte = 11
      val LamMarker: Byte = 2
      val LetMarker: Byte = 10
      val LetRecMarker: Byte = 20
      val MatchMarker: Byte = 14
      val OrMarker: Byte = 13
      val RecMarker: Byte = 9
      val RequestMarker: Byte = 16
      val SequenceMarker: Byte = 8
      val TextMarker: Byte = 7
      val UnboxedMarker: Byte = 6
      val VarMarker: Byte = 0
    }
}
