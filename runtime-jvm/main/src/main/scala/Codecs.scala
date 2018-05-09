package org.unisonweb

import util.{GraphCodec, Sequence, Sink, Source}
import GraphCodec._
import Term.Term
import Term.F._
import annotation.switch

object Codecs {

  sealed trait Node {
    def unsafeAsTerm: Term = this match {
      case Node.Term(t) => t
      case _ => sys.error("not a term: " + this)
    }
    def unsafeAsParam: Param = this match {
      case Node.Param(p) => p
      case _ => sys.error("not a param: " + this)
    }
    def fold(tf: Term => R, pf: Param => R): R = this match {
      case Node.Term(t) => tf(t)
      case Node.Param(p) => pf(p)
    }
  }

  object Node {
    case class Term(get: org.unisonweb.Term.Term) extends Node
    case class Param(get: org.unisonweb.Param) extends Node
  }

  def encodeTerm(t: Term): Sequence[Array[Byte]] =
    nodeGraphCodec.encode(Node.Term(t))

  def decodeTerm(bytes: Sequence[Array[Byte]]): Term =
    nodeGraphCodec.decode(bytes).unsafeAsTerm

  def encodeValue(p: Value): Sequence[Array[Byte]] =
    nodeGraphCodec.encode(Node.Param(p))

  def decodeValue(bytes: Sequence[Array[Byte]]): Value =
    nodeGraphCodec.decode(bytes).unsafeAsParam.toValue

  implicit val nodeGraphCodec: GraphCodec[Node] = new GraphCodec[Node] {

    import util.Pointer

    type K = Pointer

    def objectIdentity(n: Node) = n match {
      case Node.Term(t) => new Pointer(t)
      case Node.Param(p) => new Pointer(p)
    }

    def children(g: Node): Iterator[Node] = g match {
      case Node.Term(t) => t.get match {
        case ABT.Var_(_) => Iterator.empty
        case ABT.Abs_(_,t) => Iterator.single(Node.Term(t))
        case ABT.Tm_(t) => t match {
          case Compiled_(param) => Iterator.single(Node.Param(param))
          case _ =>
            (t.foldLeft(Vector.empty[Node])((r, t) => r :+ Node.Term(t))).iterator
        }
      }
      case Node.Param(p) => p match {
        case lam: Value.Lambda => Iterator.single(Node.Term(lam.decompile))
        case Value.Data(_, _, vs) => vs.iterator.map(Node.Param(_))
        case Value.EffectPure(u, b) => Iterator.single(Node.Param(b))
        case Value.EffectBind(id, cid, args, k) =>
          args.iterator.map(Node.Param(_)) ++ Iterator.single(Node.Param(k))
        case Value.Unboxed(_, _) | _ : UnboxedType => Iterator.empty
        case r: Ref => Iterator.single(Node.Param(r.value))
        case e: Builtins.External => Iterator.single(Node.Term(e.decompile))
        case t => sys.error(s"unexpected Param type ${t.getClass}")
      }
    }

    def foreachTransitive(n: Node)(f: Node => Unit): Set[util.Pointer] = {
      import util.Pointer
      Term.foreachPostorder[Node,Pointer](
        children(_),
        objectIdentity(_),
        n)(f)

    }
    def encode(sink: Sink, seen: Node => Option[Long]): Node => Unit = {
      def encodeNode(n: Node): Unit = {
        foreachTransitive(n) {
          case Node.Term(t) =>
            sink putByte 111 // more to come
            encodeTerm(t)
          case Node.Param(p) =>
            sink putByte 111
            encodeParam(p)
        }
        sink putByte 0
      }
      def encodeTerm(t: Term): Unit = seen(Node.Term(t)) match {
        case Some(pos)                      => sink putByte -99
          sink putVarLong pos

        case None => t.get match {
          case ABT.Var_(v)                  => sink putByte 0
            sink putString v.toString

          case ABT.Abs_(v,body)             => sink putByte 1
            sink putString v.toString
            encodeTerm(body)

          case ABT.Tm_(tm) => tm match {
            case Id_(id)                    => sink putByte 2
              encodeId(id, sink)

            case Constructor_(id,cid)       => sink putByte 3
              encodeId(id, sink)
              encodeConstructorId(cid, sink)

            case Request_(id,cid)           => sink putByte 4
              encodeId(id, sink)
              encodeConstructorId(cid, sink)

            case Text_(txt)                 => sink putByte 5
              sink putText txt

            case Unboxed_(u, t)             => sink putByte 6
              sink putLong u
              encodeUnboxedType(t, sink)

            case Sequence_(s)               => sink putByte 7
              sink putVarLong s.size
              s foreach (encodeTerm)

            case Lam_(b)                    => sink putByte 8
              encodeTerm(b)

            case Apply_(fn,args)            => sink putByte 9
              encodeTerm(fn)
              sink.putFramedSeq1(args)(encodeTerm)

            case Rec_(r)                    => sink putByte 10
              encodeTerm(r)

            case Let_(b,body)               => sink putByte 11
              encodeTerm(b)
              encodeTerm(body)

            case If_(cond,t,f)              => sink putByte 12
              encodeTerm(cond)
              encodeTerm(t)
              encodeTerm(f)

            case And_(x,y)                  => sink putByte 13
              encodeTerm(x)
              encodeTerm(y)

            case Or_(x,y)                   => sink putByte 14
              encodeTerm(x)
              encodeTerm(y)

            case Match_(s,cases)            => sink putByte 15
              encodeTerm(s)
              sink.putFramedSeq1(cases) { c =>
                encodePattern(c.pattern, sink)
                sink.putOption1(c.guard)(encodeTerm)
                encodeTerm(c.body)
              }

            case Handle_(h,body)            => sink putByte 16
              encodeTerm(h)
              encodeTerm(body)

            case EffectPure_(t)             => sink putByte 17
              encodeTerm(t)

            case EffectBind_(id,cid,args,k) => sink putByte 18
              encodeId(id, sink)
              encodeConstructorId(cid, sink)
              sink.putFramedSeq1(args)(encodeTerm)
              encodeTerm(k)

            case LetRec_(bs,b)              => sink putByte 19
              sink.putFramedSeq1(bs)(encodeTerm)
              encodeTerm(b)

            case Compiled_(p)               => sink putByte 20
              encodeParam(p)
          }
        }
      }

      def encodeParam(p: Param): Unit = seen(Node.Param(p)) match {
        case Some(pos)                          => sink putByte -99
          sink putVarLong pos

        case None => p match {
          case Value.Unboxed(u, typ)             => sink putByte 21
            sink.putLong(u)
            encodeUnboxedType(typ, sink)

          case lam: Value.Lambda                 => sink putByte 22
            encodeTerm(lam.decompile)

          case d@Value.Data(id, cid, vs)         => sink putByte 23
            encodeId(id, sink)
            encodeConstructorId(cid, sink)
            sink.putFramedSeq1(vs)(encodeParam)

          case Value.EffectPure(u, b)            => sink putByte 24
            sink putLong u
            encodeParam(b)

          case Value.EffectBind(id,cid, args, k) => sink putByte 25
            encodeId(id, sink)
            encodeConstructorId(cid, sink)
            sink.putFramedSeq1(args)(encodeParam)
            encodeParam(k)

          case r: Ref                            => sink putByte 26
            sink putString r.name.toString
            encodeParam(r.value)

          case e: Builtins.External              => sink putByte 27
            encodeTerm(e.decompile)

          case UnboxedType.Boolean               => sink putByte 28
          case UnboxedType.Int64                 => sink putByte 29
          case UnboxedType.UInt64                => sink putByte 30
          case UnboxedType.Float                 => sink putByte 31
          case t =>
            sys.error(s"unexpected Param type ${t.getClass} in encodeParam")
        }
      }

      encodeNode(_)
    }

    def decode(src: Source,
               seen: Long => Option[Node],
               done: (Position,Node) => Unit): () => Node = {

      // used for evaluation of lambdas and externals, whose
      // wire format is the decompiled form that must be
      // re-evaluated by the decoder
      val stackU = new Array[U](512)
      val stackB = new Array[B](512)
      val R = compilation.Result()

      def decode: Node = {
        // the last node in the stream is the root node -
        // it a flat expression that refers back to earlier nodes in the stream
        var last: Node = null
        src.foreachDelimited(decode0) { node => last = node }
        last
      }

      def decode0: Node = {
        val pos = src.position
        val tag = src.getByte
        // todo: this shouldn't need to be duplicated?
        if (tag == -99) {
          // magic byte indicating a backref to a position follows
          val i = src.getVarLong
          seen(i) match {
            case Some(n) => done(pos, n); n
            case None => sys.error("unknown reference to position: " + i)
          }
        }
        else if (tag <= 20) Node.Term(decodeTerm(pos, tag))
        else Node.Param(decodeParam(pos, tag))
      }

      def decodeTerm0: Term = decodeTerm(src.position, src.getByte)
      def decodeTerm(pos: Position, tag: Byte): Term = {
        val t: Term = (tag: @switch) match {
          case -99 => val i = src.getVarLong; seen(i) match {
            case Some(n) => n.unsafeAsTerm
            case None => sys.error("unknown reference to position: " + i)
          }
          case 0  => ABT.Var(src.getString)
          case 1  => ABT.Abs(src.getString, decodeTerm0)
          case 2  => Term.Id(decodeId(src))
          case 3  => Term.Constructor(decodeId(src), decodeConstructorId(src))
          case 4  => Term.Request(decodeId(src), decodeConstructorId(src))
          case 5  => Term.Text(src.getText)
          case 6  => Term.Unboxed(src.getLong, decodeUnboxedType(src))
          case 7  => Term.Sequence(src.getFramedSequence1(decodeTerm0))
          case 8  => ABT.Tm(Lam_(decodeTerm0))
          case 9  => Term.Apply(decodeTerm0, src.getFramedList1(decodeTerm0):_*)
          case 10 => ABT.Tm(Rec_(decodeTerm0))
          case 11 => ABT.Tm(Let_(decodeTerm0, decodeTerm0))
          case 12 => Term.If(decodeTerm0, decodeTerm0, decodeTerm0)
          case 13 => Term.And(decodeTerm0, decodeTerm0)
          case 14 => Term.Or(decodeTerm0, decodeTerm0)
          case 15 => /* Match */
            val scrutinee = decodeTerm0
            val cases = src.getFramedList1 {
              Term.MatchCase(
                decodePattern(src),
                src.getOption1(decodeTerm0),
                decodeTerm0
              )
            }
            Term.Match(scrutinee)(cases: _*)
          case 16 => Term.Handle(decodeTerm0)(decodeTerm0)
          case 17 => Term.EffectPure(decodeTerm0)
          case 18 => Term.EffectBind(
            decodeId(src),
            decodeConstructorId(src),
            src.getFramedList1(decodeTerm0),
            decodeTerm0)
          case 19 =>
            ABT.Tm(LetRec_(src.getFramedList1(decodeTerm0), decodeTerm0))
          case 20 =>
            Term.Compiled(decodeParam0)
        }
        done(pos, Node.Term(t))
        t
      }
      def decodeParam0: Param = decodeParam(src.position, src.getByte)
      def decodeParam(pos: Position, tag: Byte): Param = {
        val currentRef = new Ref("v"+pos, null)
        done(pos, Node.Param(currentRef))
        val p: Param = (tag: @switch) match {
          case -99 => val i = src.getVarLong; seen(i) match {
            case Some(n) => n.unsafeAsParam
            case None => sys.error("unknown reference to position: " + i)
          }
          case 21 => Value.Unboxed(src.getLong, decodeUnboxedType(src))
          case 22 => /* Lambda */
            // in order to do compilation we need the compilation environment
            val c = compilation.compileTop(Environment.standard)(decodeTerm0)
            val sp0 = compilation.StackPtr.empty
            Value(compilation.evalClosed(c,R,sp0,stackU,stackB), R.boxed)
          case 23 => Value.Data(decodeId(src),
                                decodeConstructorId(src),
                                src.getFramedArray1(decodeParam0.toValue))
          case 24 => Value.EffectPure(src.getLong, decodeParam0.toValue)
          case 25 => Value.EffectBind(
            decodeId(src), decodeConstructorId(src),
            src.getFramedArray1(decodeParam0.toValue),
            decodeParam0.toValue.asInstanceOf[Value.Lambda])
          case 26 =>
            // tricky - we declare this `Ref` done before
            // deserializing the value inside. This allows the
            // `Ref` to refer to itself.
            val ref = new Ref(src.getString, null)
            done(pos, Node.Param(ref))
            ref.value = decodeParam0.toValue
            ref
          case 27 => /* External */
            // in order to do compilation we need the compilation environment
            val c = compilation.compileTop(Environment.standard)(decodeTerm0)
            val sp0 = compilation.StackPtr.empty
            Value(compilation.evalClosed(c,R,sp0,stackU,stackB), R.boxed)
          case 28 => UnboxedType.Boolean
          case 29 => UnboxedType.Int64
          case 30 => UnboxedType.UInt64
          case 31 => UnboxedType.Float
          case t =>
            sys.error(s"unexpected tag byte $t during decoding")
        }
        currentRef.value = p.toValue
        done(pos, Node.Param(p))
        p
      }
      () => decode
    }
  }

  final def decodeId(source: Source): Id = (source.getByte: @switch) match {
    case 0 => Id.Builtin(source.getString)
    case 1 => Id.HashRef(Hash(source.getFramed))
  }

  final def encodeId(id: Id, sink: Sink): Unit = id match {
    case Id.Builtin(name) =>
      sink putByte 0
      sink putString name.toString
    case Id.HashRef(h) =>
      sink putByte 1
      sink putFramed h.bytes
  }

  final def decodeConstructorId(source: Source): ConstructorId =
    ConstructorId(source.getInt)

  final def encodeConstructorId(cid: ConstructorId, sink: Sink): Unit =
    sink putInt cid.toInt

  final def decodeUnboxedType(source: Source): UnboxedType =
    (source.getByte: @switch) match {
      case 0 => UnboxedType.Boolean
      case 1 => UnboxedType.Int64
      case 2 => UnboxedType.UInt64
      case 3 => UnboxedType.Float
    }

  final def encodeUnboxedType(t: UnboxedType, sink: Sink): Unit = t match {
    case UnboxedType.Boolean => sink.putByte(0)
    case UnboxedType.Int64 => sink.putByte(1)
    case UnboxedType.UInt64 => sink.putByte(2)
    case UnboxedType.Float => sink.putByte(3)
  }

  final def decodePattern(src: Source): Pattern = (src.getByte: @switch) match {
    case 0 => Pattern.LiteralU(src.getLong, decodeUnboxedType(src))
    case 1 => Pattern.Wildcard
    case 2 => Pattern.Uncaptured
    case 3 => Pattern.Data(decodeId(src), decodeConstructorId(src),
                           src.getFramedList(decodePattern))
    case 4 => Pattern.As(decodePattern(src))
    case 5 => Pattern.EffectPure(decodePattern(src))
    case 6 => Pattern.EffectBind(decodeId(src), decodeConstructorId(src),
                                 src.getFramedList(decodePattern),
                                 decodePattern(src))
  }

  final def encodePattern(p: Pattern, sink: Sink): Unit = p match {
    case Pattern.LiteralU(u, typ)      => sink putByte 0
      sink.putLong(u)
      encodeUnboxedType(typ, sink)

    case Pattern.Wildcard              => sink putByte 1
    case Pattern.Uncaptured            => sink putByte 2

    case Pattern.Data(id,cid,patterns) => sink putByte 3
      encodeId(id, sink)
      encodeConstructorId(cid, sink)
      sink.putFramedSeq1(patterns)(encodePattern(_,sink))

    case Pattern.As(p)                 => sink putByte 4
      encodePattern(p, sink)

    case Pattern.EffectPure(p)         => sink putByte 5
      encodePattern(p, sink)

    case Pattern.EffectBind(
      id,cid,patterns,continuation)    => sink putByte 6
        encodeId(id, sink)
        encodeConstructorId(cid, sink)
        sink.putFramedSeq(patterns)(
          (sink,p) => encodePattern(p,sink))
        encodePattern(continuation, sink)
  }
}
