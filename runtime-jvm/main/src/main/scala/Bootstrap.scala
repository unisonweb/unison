package org.unisonweb

import util._
import BuiltinTypes._
import ABT.Name
import compilation._

object Bootstrap {
  def main(args: Array[String]): Unit = {
    import Codecs._

    val fileName = args(0)
    val src = Source.fromFile(fileName)
    val data = decodeConstructorArities(src)
    val effects = decodeConstructorArities(src)
    val term = termDecoder(src)

    val datas = data.flatMap { case (id, arities) =>
      arities.zipWithIndex.map {
        case (arity, cid) =>
          dataConstructor(id, ConstructorId(cid),
                          Range(0,arity).map(x => Name("x" + x)):_*)
      }
    }.toMap

    val effectss = effects.flatMap { case (id, arities) =>
      arities.zipWithIndex.map {
        case (arity, cid) =>
          effectRequest(id, ConstructorId(cid),
                        Range(0,arity).map(x => Name("x" + x)):_*)
      }
    }.toMap

    val env =
      Environment.standard.copy(
        dataConstructors = Environment.standard.dataConstructors ++ datas,
        effects = Environment.standard.effects ++ effectss
      )

    println(PrettyPrint.prettyTerm(normalize(env)(term)).render(80))
  }
}
