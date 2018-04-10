package org.unisonweb

sealed class Pattern
object Pattern {
  case class LiteralU(u: U, typ: UnboxedType) extends Pattern
  case object Wildcard extends Pattern
  case object Uncaptured extends Pattern
  case class Data(typeId: Hash, constructorId: Int, patterns: List[Pattern]) extends Pattern
//  case class SequenceUncons(left: Pattern, right: Pattern) extends Pattern
}
