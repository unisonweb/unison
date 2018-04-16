package org.unisonweb

sealed abstract class Pattern(val arity: Int)

object Pattern {
  case class LiteralU(u: U, typ: UnboxedType) extends Pattern(0)
  case object Wildcard extends Pattern(1)
  case object Uncaptured extends Pattern(0)
  case class Data(typeId: Hash,
                  constructorId: ConstructorId,
                  patterns: List[Pattern])
    extends Pattern(patterns.map(_.arity).sum)
  case class As(p: Pattern) extends Pattern(1 + p.arity)
  case class EffectPure(p: Pattern) extends Pattern(p.arity)
  case class EffectBind(id: Id, ctor: ConstructorId, patterns: List[Pattern])
    extends Pattern(patterns.map(_.arity).sum)
//  case class SequenceUncons(left: Pattern, right: Pattern) extends Pattern

  def Tuple(xs: Pattern*): Pattern =
    Data(Hash(null), ConstructorId(0), xs.toList)
}
