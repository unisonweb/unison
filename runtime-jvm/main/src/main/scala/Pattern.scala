package org.unisonweb

sealed abstract class Pattern
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
