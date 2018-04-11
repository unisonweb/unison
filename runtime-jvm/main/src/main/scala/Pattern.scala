package org.unisonweb

sealed abstract class Pattern {
  import Pattern._

  def arity: Int = this match {
    case LiteralU(_,_) => 0
    case Wildcard => 1
    case Uncaptured => 0
    case As(p) => 1 + p.arity
    case Data(_,_,ps) => ps.map(_.arity).sum
  }
}

object Pattern {
  case class LiteralU(u: U, typ: UnboxedType) extends Pattern
  case object Wildcard extends Pattern
  case object Uncaptured extends Pattern
  case class Data(typeId: Hash,
                  constructorId: ConstructorId,
                  patterns: List[Pattern]) extends Pattern
  case class As(p: Pattern) extends Pattern
//  case class SequenceUncons(left: Pattern, right: Pattern) extends Pattern
}
