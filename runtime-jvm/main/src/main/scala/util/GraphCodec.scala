package org.unisonweb.util

/** Functions for encoding/decoding object graphs with cycles. */
object GraphCodec {

  type Position = Long

  /**
   * A flattened format for a graph `G`. Satisfies the
   * invariants that:
   *
   *   - `SetRef i j` must be at a position greater than
   *     both `i` and `j` (we can only set references
   *     already declared)
   *
   *   - An `Emit(g)` at position `i` can only reference
   *     `G` values emitted at positions `<= i` (we can
   *     only refer back to previously emitted values).
   *
   * The output is completely flat. Example, the lambda `x -> 79`:
   *
   *   0	Unboxed(79,Int64)
   *   1	Compiled_(Unboxed(79,Int64))
   *   2	Abs_(x,@1)
   *   3	Lam_(@2)
   */
  case class Format[G](
    instructions: Sequence[Instruction[G]],
    positionOf: G => Position,
    root: G
  )

  trait Instruction[+G]

  object Instruction {
    case class Emit[G](g: G) extends Instruction[G]
    case class SetRef(ref: Position, referent: Position) extends Instruction[Nothing]
  }
  import Instruction._

  /**
   * Produce an encoder for graph type `G`:
   *
   * - `children` iterates over the child nodes of a `G`.
   * - `id` extracts a key used to uniquely identify nodes.
   * - `isRef` is `true` if a node is a mutable reference
   * - `referent` dereferences a reference node
   *
   * For any node, `g`, if `isRef(g)`, then `children(g)`
   * should include `referent(g)`.
   */
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

    // Algorithm works in 3 passes
    // 1. Emit/declare all the refs transitively reachable from root
    foreachPostorder(Set.empty, children, id, root) { g => if (isRef(g)) emit(g) }
    // 2. Emit all the referents of these refs
    val seen = out.foldLeft(posOf.keys.toSet) {
      case (seen,Emit(ref)) =>
        val seen2 = foreachPostorder(seen, skipRefs, id, referent(ref))(emit)
        // after each referent is encoded, we issue a SetRef instruction
        out = out :+ SetRef(posOf(id(ref)), out.size - 1)
        seen2
      case _ => sys.error("unpossible")
    }
    // 3. Emit the root node
    foreachPostorder(seen, skipRefs, id, root)(emit)
    // We ensure root node is emitted last, even if previously emitted
    out.lastOption match {
      case Some(Emit(e)) if id(e) == id(root) => ()
      case _ => emit(root)
    }
    Format(out, (g: G) => posOf(id(g)), root)
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
    sink putVarLong fmt.positionOf(fmt.root)
  }

  def decodeSource[G](src: Source)(
    // setRef(ref, referent)
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
        case 0 =>
          decoded(src.getVarLong)
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
