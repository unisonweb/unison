package org.unisonweb

import util.GraphCodec2
import GraphCodec2.Done
import util.{Sink, Source}
import Term.Term
// import Term.F._

object Codecs2 {

  sealed trait Node {
    def unsafeAsTerm: Term = this match {
      case Node.Term(t) => t
      case _ => sys.error("not a term: " + this)
    }
    def unsafeAsParam: Param = this match {
      case Node.Param(p) => p
      case _ => sys.error("not a param: " + this)
    }
  }

  object Node {
    case class Term(get: org.unisonweb.Term.Term) extends Node
    case class Param(get: org.unisonweb.Param) extends Node
  }

  implicit val nodeGraphCodec: GraphCodec2[Node] = new GraphCodec2[Node] {
    def encode(sink: Sink, seen: Node => Option[Long]): Node => Unit = ???
    def decode(src: Source, seen: Long => Option[Node], done: Node => Node with Done): () => Node with Done = ???
  }

}
