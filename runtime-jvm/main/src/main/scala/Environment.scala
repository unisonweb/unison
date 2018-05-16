package org.unisonweb

import compilation._
import Term.Name

case class Environment(
  builtins: Name => Computation,
  userDefined: Hash => Computation,
  dataConstructors: (Id,ConstructorId) => Computation,
  effects: (Id,ConstructorId) => Computation
)

object Environment {

  val standard = Environment(
    Builtins.builtins,
    userDefined = _ => ???,
    BuiltinTypes.dataConstructors,
    BuiltinTypes.effects)
}
