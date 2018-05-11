package org.unisonweb.util

object GraphCodec {

  type Position = Long

  trait Instruction[+G]

  object Instruction {
    case class Emit[G](g: G) extends Instruction[G]
    case class SetRef(ref: Position, referent: Position) extends Instruction[Nothing]
  }
  import Instruction._

  case class Format[G](
    instructions: Sequence[Instruction[G]],
    positionOf: G => Position
  )

  def encoder[G,K](
    children: G => Iterator[G], id: G => K,
    isRef: G => Boolean, referent: G => G): G => Format[G] = root => {

    var out = Sequence.empty[Instruction[G]]
    val posOf = new collection.mutable.HashMap[K,Long]()
    def emit(g: G) = {
      if (!posOf.contains(id(g))) posOf.update(id(g), out.size)
      out = out :+ Emit(g)
    }
    def skipRefs(g: G) = if (isRef(g)) Iterator.empty else children(g)

    foreachPostorder(Set.empty, children, id, root) { g => if (isRef(g)) emit(g) }
    val seen = out.foldLeft(posOf.keys.toSet) {
      case (seen,Emit(ref)) =>
        val seen2 = foreachPostorder(seen, skipRefs, id, referent(ref))(emit)
        out = out :+ SetRef(posOf(id(ref)), out.size - 1)
        seen2
      case _ => sys.error("unpossible")
    }
    foreachPostorder(seen - id(root), skipRefs, id, root)(emit)
    out.lastOption match {
      case Some(Emit(e)) if id(e) == id(root) => ()
      case _ => emit(root)
    }
    Format(out, (g: G) => posOf(id(g)))
  }

  def encodeSink[G](sink: Sink, fmt: Format[G])
                   (emitter: (Sink, G => Position) => G => Unit): Unit = {
    val emitter1 = emitter(sink, fmt.positionOf)
    fmt.instructions.foreach { i =>
      sink putByte 111 // more instructions
      i match {
        case Emit(g) => sink putByte 0; emitter1(g)
        case SetRef(ref, referent) =>
          sink putByte 1
          sink putVarLong ref
          sink putVarLong referent
      }
    }
    sink putByte 0 // end of stream marker
  }

  def decodeSource[G](src: Source)(
    setRef: (G, G) => Unit,
    decode: (Source, Position => G) => () => G): G = {

    val decoded = collection.mutable.LongMap.empty[G]
    val decode1 = decode(src, decoded)
    var last: Option[G] = None

    @annotation.tailrec def go(pos: Position): G =
      src.getByte match {
        case 111 =>
          src.getByte match {
            case 0 => val g = decode1(); last = Some(g); decoded.update(pos, g)
            case 1 => setRef(decoded(src.getVarLong), decoded(src.getVarLong))
          }
          go(pos + 1)
        case 0 => last.getOrElse(sys.error("empty stream"))
      }
    go(0)
  }

  def foreachPostorder[G,K](
    seen: Set[K],
    children: G => Iterator[G],
    id: G => K,
    g: G)(
    f: G => Unit): Set[K] = {

    @annotation.tailrec
    def go(seen: Set[K], g: Option[G], rem: Sequence[Either[() => Unit, G]]): Set[K] = {
      g match {
        case None => rem.uncons match {
          case None => seen
          case Some((e,rem)) => e match {
            case Left(thunk) =>
              thunk()
              go(seen, None, rem)
            case Right(g) => go(seen, Some(g), rem)
          }
        }
        case Some(g) =>
          if (seen.contains(id(g))) go(seen, None, rem)
          else (seen + id(g)) match { // we haven't seen this node before
            case seen =>
              val rem2 = Left(() => f(g)) +: rem
              go(seen, None,
              children(g).foldRight(rem2)((child,rem) => Right(child) +: rem))
          }
      }
    }
    go(seen, Some(g), Sequence.empty[Either[() => Unit, G]])
  }
}
