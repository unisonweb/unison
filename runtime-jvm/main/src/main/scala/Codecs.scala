package org.unisonweb

import util.{GraphCodec, Sequence, Sink, Source, Pointer}
import GraphCodec._
import Term.Term
import Term.F._
import annotation.switch

object Codecs {

  sealed trait Node {
    def toPointer: Pointer = fold(t => new Pointer(t), p => new Pointer(p))

    def unsafeAsTerm: Term = this match {
      case Node.Term(t) => t
      case _ => sys.error("not a term: " + this)
    }
    def unsafeAsParam: Param = this match {
      case Node.Param(p) => p
      case _ => sys.error("not a param: " + this)
    }
    def fold[R](tf: Term => R, pf: Param => R): R = this match {
      case Node.Term(t) => tf(t)
      case Node.Param(p) => pf(p)
    }
  }

  object Node {
    case class Term(get: org.unisonweb.Term.Term) extends Node
    case class Param(get: org.unisonweb.Param) extends Node
  }

  val nodeEncoder: Node => Format[Node] =
    encoder(children, objectIdentity, isRef, referent)

  val nodeDecoder: Source => Node =
    src => decodeSource(src)(setRef, decoder)

  def termDecoder(src: Source): Term =
    nodeDecoder(src).unsafeAsTerm

  def encodeNode(n: Node): Sequence[Array[Byte]] = {
    val fmt = nodeEncoder(n)
    // println(prettyFormat(fmt))
    Sink.toChunks(4096) { sink => encodeSink(sink, fmt)(emitter) }
  }

  def encodeTerm(t: Term): Sequence[Array[Byte]] = encodeNode(Node.Term(t))
  def encodeValue(p: Param): Sequence[Array[Byte]] = encodeNode(Node.Param(p))

  def decodeNode(bs: Sequence[Array[Byte]]): Node =
    decodeSource(Source.fromChunks(4096)(bs))(setRef, decoder)

  def decodeTerm(bs: Sequence[Array[Byte]]): Term =
    decodeNode(bs).unsafeAsTerm

  def decodeValue(bs: Sequence[Array[Byte]]): Value =
    decodeNode(bs).unsafeAsParam.toValue

  def objectIdentity(n: Node) = n match {
    case Node.Term(t) => new Pointer(t)
    case Node.Param(p) => new Pointer(p)
  }

  def isRef(n: Node) = n match {
    case Node.Param(r : Ref) => true
    case _ => false
  }

  def setRef(ref: Node, referent: Node) = {
    ref.unsafeAsParam.asInstanceOf[Ref].value = referent.unsafeAsParam.toValue
  }

  def referent(n: Node): Node =
    Node.Param(n.unsafeAsParam.asInstanceOf[Ref].value)

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

  def emitter(sink: Sink, positionOf: Node => Position): Node => Unit = {
    // These functions are used for encoding children of a Node
    // Since any child of a node will have been previously encoded,
    // we just emit a backreference to the position where it is encoded
    def encode(t: Term): Unit  = sink.putVarLong(positionOf(Node.Term(t)))
    def encodep(p: Param): Unit = sink.putVarLong(positionOf(Node.Param(p)))

    def emitTerm(t: Term): Unit = t.get match {
      case ABT.Var_(v)
          => sink putByte 0; sink putString v.toString
      case ABT.Abs_(v,body)
          => sink putByte 1; sink putString v.toString; encode(body)
      case ABT.Tm_(tm) => tm match {
        case Id_(id)
          => sink putByte 2; encodeId(id, sink)
        case Constructor_(id,cid)
          => sink putByte 3; encodeId(id, sink); encodeConstructorId(cid, sink)
        case Request_(id,cid)
          => sink putByte 4; encodeId(id, sink); encodeConstructorId(cid, sink)
        case Text_(txt)
          => sink putByte 5; sink putText txt
        case Unboxed_(u, t)
          => sink putByte 6; sink putLong u; encodeUnboxedType(t, sink)
        case Sequence_(s)
          => sink putByte 7; sink.putFramedSequence1(s)(encode)
        case Lam_(b)
          => sink putByte 8; encode(b)
        case Apply_(fn,args)
          => sink putByte 9; encode(fn); sink.putFramedSeq1(args)(encode)
        case Rec_(r)
          => sink putByte 10; encode(r)
        case Let_(b,body)
          => sink putByte 11; encode(b); encode(body)
        case If_(cond,t,f)
          => sink putByte 12; encode(cond); encode(t); encode(f)
        case And_(x,y)
          => sink putByte 13; encode(x); encode(y)
        case Or_(x,y)
          => sink putByte 14; encode(x); encode(y)
        case Match_(s,cases)
          => sink putByte 15; encode(s)
             sink.putFramedSeq1(cases) { c =>
               encodePattern(c.pattern, sink)
               sink.putOption1(c.guard)(encode)
               encode(c.body)
             }
        case Handle_(h,body)
          => sink putByte 16; encode(h); encode(body)
        case EffectPure_(t)
          => sink putByte 17; encode(t)
        case EffectBind_(id,cid,args,k)
          => sink putByte 18
             encodeId(id, sink)
             encodeConstructorId(cid, sink)
             sink.putFramedSeq1(args)(encode)
             encode(k)
        case LetRec_(bs,b)
          => sink putByte 19; sink.putFramedSeq1(bs)(encode); encode(b)
        case Compiled_(p)
          => sink putByte 20; encodep(p)
      }
    }

    def emitParam(p: Param): Unit = p match {
      case Value.Unboxed(u, typ)
        => sink putByte 21; sink.putLong(u); encodeUnboxedType(typ, sink)
      case lam: Value.Lambda
        => sink putByte 22; encode(lam.decompile)
      case d@Value.Data(id, cid, vs)
        => sink putByte 23; encodeId(id, sink); encodeConstructorId(cid, sink)
           sink.putFramedSeq1(vs)(encodep)
      case Value.EffectPure(u, b)
        => sink putByte 24; sink putLong u; encodep(b)
      case Value.EffectBind(id,cid, args, k)
        => sink putByte 25; encodeId(id, sink); encodeConstructorId(cid, sink)
           sink.putFramedSeq1(args)(encodep)
           encodep(k)
      case r: Ref
        => sink putByte 26; sink putString r.name.toString
      case e: Builtins.External
        => sink putByte 27; encode(e.decompile)
      case UnboxedType.Boolean
        => sink putByte 28
      case UnboxedType.Int64
        => sink putByte 29
      case UnboxedType.Nat
        => sink putByte 30
      case UnboxedType.Float
        => sink putByte 31
      case t => sys.error(s"unexpected Param type ${t.getClass} in encodeParam")
    }
    _ match {
      case Node.Term(t) => emitTerm(t)
      case Node.Param(p) => emitParam(p)
    }
  }

  def decoder(src: Source, at: Position => Node): () => Node = {
    // used for evaluation of lambdas and externals, whose
    // wire format is the decompiled form that must be
    // re-evaluated by the decoder
    val stackU = new Array[U](512)
    val stackB = new Array[B](512)
    val R = compilation.Result()
    def decodeTerm0: Term = at(src.getVarLong).unsafeAsTerm
    def decodeParam0: Param = at(src.getVarLong).unsafeAsParam
    def decodeTerm(tag: Byte): Term = tag match {
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

    def decodeParam(tag: Byte): Param = (tag : @switch) match {
      case 21 => Value.Unboxed(src.getLong, decodeUnboxedType(src))
      case 22 => /* Lambda */
        // in order to do compilation we need the compilation environment
        val c = compilation.compileTop(Environment.standard)(decodeTerm0)
        val sp0 = compilation.StackPtr.empty
        val u = compilation.evalClosed(c,R,sp0,stackU,stackB)
        Value(u, R.boxed)
      case 23 => Value.Data(decodeId(src),
                            decodeConstructorId(src),
                            src.getFramedArray1(decodeParam0.toValue))
      case 24 => Value.EffectPure(src.getLong, decodeParam0.toValue)
      case 25 => Value.EffectBind(
        decodeId(src), decodeConstructorId(src),
        src.getFramedArray1(decodeParam0.toValue),
        decodeParam0.toValue.asInstanceOf[Value.Lambda])
      case 26 =>
        // Refs are read in the empty state and have referent set later
        new Ref(src.getString, null)
      case 27 => /* External */
        // in order to do compilation we need the compilation environment
        val c = compilation.compileTop(Environment.standard)(decodeTerm0)
        val sp0 = compilation.StackPtr.empty
        val u = compilation.evalClosed(c,R,sp0,stackU,stackB)
        Value(u, R.boxed)
      case 28 => UnboxedType.Boolean
      case 29 => UnboxedType.Int64
      case 30 => UnboxedType.Nat
      case 31 => UnboxedType.Float
      case t => sys.error(s"unexpected tag byte $t during decoding")
    }

    () => {
      val tag = src.getByte
      if (tag <= 20) Node.Term(decodeTerm(tag))
      else Node.Param(decodeParam(tag))
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

  final def decodeConstructorArities(source: Source): List[(Id, List[Int])] =
    source.getFramedList1 {
      val id = decodeId(source)
      val arities = source.getFramedList1(source.getInt)
      (id, arities)
    }

  final def decodeConstructorId(source: Source): ConstructorId =
    ConstructorId(source.getInt)

  final def encodeConstructorId(cid: ConstructorId, sink: Sink): Unit =
    sink putInt cid.toInt

  final def decodeUnboxedType(source: Source): UnboxedType =
    (source.getByte: @switch) match {
      case 0 => UnboxedType.Boolean
      case 1 => UnboxedType.Int64
      case 2 => UnboxedType.Nat
      case 3 => UnboxedType.Float
    }

  final def encodeUnboxedType(t: UnboxedType, sink: Sink): Unit = t match {
    case UnboxedType.Boolean => sink.putByte(0)
    case UnboxedType.Int64 => sink.putByte(1)
    case UnboxedType.Nat => sink.putByte(2)
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

  def prettyFormat(f: Format[Node]): String = {
    import GraphCodec.Instruction._
    def backref(t: Term) = "@"+f.positionOf(Node.Term(t)).toString
    def backrefp(p: Param) = "@"+f.positionOf(Node.Param(p)).toString
    f.instructions.toList.zipWithIndex.foldLeft(Vector("-----")) {
      case (buf,(s,i)) =>
        val line = s"$i\t" + { s match {
          case Emit(n) => n match {
            case Node.Term(Term.Compiled(p)) => s"Compiled(${backrefp(p)})"
            case Node.Term(t) => t.get.map(backref).toString
            case Node.Param(p) => p match {
              case lam : Value.Lambda => "Lambda " + backref(lam.decompile)
              case e: Builtins.External => "External " + backref(e.decompile)
              case Value.Data(id,cid,fields) =>
                s"Data $id $cid ${fields.map(backrefp).mkString(" ")}"
              case Value.EffectPure(u,b) =>
                s"EffectPure $u ${backrefp(b)}"
              case Value.EffectBind(id,cid,fields,k) =>
                s"EffectBind $id $cid ${fields.map(backrefp).mkString(" ")} | ${backrefp(k)}"
              case _ => p.toString
            }
          }
          case SetRef(p1, p2) => s"SetRef $p1 $p2"
        }}
        buf :+ line
    }.mkString("\n") + "\n" + "@"+f.positionOf(f.root).toString
  }

  final def prettyEncoding(bytes: util.Sequence[Array[Byte]]): String =
    prettyEncoding(Source.fromChunks(4096)(bytes)).mkString("\n")

  final def prettyEncoding(bytes: Source): Vector[String] = {
    def go(bs: Source): String = {
      def r = go(bs)
      val s = bs.getByte match {
        case (-99) => s"@${bs.getVarLong}"
        case 0 => s"Var ${bs.getString}"
        case 1 => s"Abs ${bs.getString} $r"
        case 2 => decodeId(bs).toString
        case 3 => s"Constructor ${decodeId(bs)} ${decodeConstructorId(bs)}"
        case 4 => s"Request ${decodeId(bs)} ${decodeConstructorId(bs)}"
        case 5 => s"Text ${bs.getText}"
        case 6 => s"Unboxed ${bs.getLong}:${decodeUnboxedType(bs)}"
        case 7 =>
          val seq = bs.getFramedList(go _)
          s"Sequence(${seq.mkString(", ")})"
        case 8 => s"Lam $r"
        case 9 =>
          val fn = go(bs)
          val args = bs.getFramedList(go)
          s"App $fn ${args.mkString(" ")}"
        case 10 => s"Rec $r"
        case 11 => s"Let $r $r"
        case 12 => s"If $r $r $r"
        case 13 => s"And $r $r"
        case 14 => s"Or $r $r"
        case 15 =>
          val scrutinee = go(bs)
          val cases = bs.getFramedList(???)
          s"Match $scrutinee $cases"
        case 16 => s"Handle $r $r"
        case 17 => s"EffectPure $r"
        case 18 =>
          val id = decodeId(bs)
          val ctor = decodeConstructorId(bs)
          val args = bs.getFramedList(go)
          val k = go(bs)
          s"EffectBind $id $ctor(${args.mkString(", ")}) $k"
        case 19 =>
          val bindings = bs.getFramedList(go)
          s"LetRec ${bindings.mkString(" ")}\n  $r"
        case 20 => s"Compiled $r"
        case 21 => s"Value.Unboxed ${bs.getLong}:${decodeUnboxedType(bs)}"
        case 22 => s"Value.Lambda $r"
        case 23 =>
          val id = decodeId(bs)
          val ctor = decodeConstructorId(bs)
          val args = bs.getFramedList(go)
          s"Data $id $ctor(${args.mkString(", ")})"
        case 24 =>
          val unboxed = bs.getLong
          val boxed = go(bs)
          s"Value.EffectPure ($unboxed)($boxed)"
        case 25 => ??? // TODO
        case 26 =>
          s"Ref ${bs.getString}"
        case 27 =>
          s"External $r"
        case 28 => "Boolean"
        case 29 => "Int64"
        case 30 => "Nat"
        case 31 => "Float"
        case 32 => s"SetRef ${bs.getVarLong} ${bs.getVarLong}"
      }
      s
    }

    var r = Vector[String]()
    bytes.foreachDelimited((bytes.position, go(bytes))) {
      case (pos, s) =>
        r = r :+ (pos.toString + ":\t" + s)
    }
    r
  }
}

