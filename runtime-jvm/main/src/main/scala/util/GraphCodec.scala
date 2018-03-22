//package org.unisonweb.util
//
//import scala.collection.mutable
//import java.util.IdentityHashMap
//
//object GraphCodec {
//
//  type Position = Long
//
//  trait Graphstream {
//    def byteCount: Long
//  }
//
//  object Graphstream {
//
//    trait Nested {
//      def bytePrefix: Array[Byte]
//
//      /**
//       * `f` receives the accumulated `r`, the current child `Graphstream`,
//       * and the position of that `Graphstream`. The first child has a position
//       * of `start + bytePrefix.length`.
//       */
//      def childGraphs[R](r: R)(f: (R,Graphstream) => R): R
//
//      lazy val byteCount: Long =
//        childGraphs(prefix.length)(prefix.length.toLong)((r,pos,g) => r + g.byteCount)
//    }
//
//    /** A reference to an earlier position in the stream. */
//    // just need a way of refering to the Ref, could be anything, but ideally not
//    // based on pointer equality - actually, that is perfectly fine - it is later
//    // in serialization that we convert to numbered positions
//    trait Ref extends Graphstream {
//      def referencePosition: Position
//      def byteCount = 8L
//    }
//  }
//
//  // todo - figure out how childGraphs works
//
//  trait Cyclic[A] { def cyclicFold[R](pure: A => R, ref: (Name, => A) => R): R }
//
//  def encode[A](expand: A => (Array[Byte], Array[A])): A => Graphstream =
//    root => {
//      import Graphstream._
//      val m = new IdentityHashMap[A,Position]
//      def ref(pos: Position): Array[Byte] = ??? // todo
//
//      def go(pos: Position, a: A): Graphstream = {
//        val pos0 = m.get(a)
//        if (pos0 eq null) {
//          m.put(a, pos)
//          val (prefix0, children) = expand(a)
//          var pos1 = pos
//          Nested(prefix0, children.map(a => { val s = go(pos, a); pos1 += s.size; s }))
//        }
//        else Ref(pos0)
//      }
//      go(0, root)
//    }
//
//  def decode[A](make: (Array[Byte], Sequence[A]) => A): Graphstream => A = {
//    val m = new mutable.HashMap[Position,A]
//    def go(g: Graphstream): A = g match {
//      case Graphstream.Ref(pos) =>
//        m.getOrElse(pos, sys.error("reference to nonexistent position"))
//      case Graphstream.Nested(prefix, children) =>
//        make(prefix, children map (go))
//    }
//    go(_)
//  }
//}
