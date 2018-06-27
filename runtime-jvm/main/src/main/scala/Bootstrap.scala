package org.unisonweb

import org.unisonweb.ABT.Name
import org.unisonweb.BuiltinTypes._
import org.unisonweb.compilation._
import org.unisonweb.util._

object Bootstrap {
  def main(args: Array[String]): Unit = {
    import Codecs._

    val fileName = args match {
      case Array(s) => s
      case _ =>
        println("usage: Bootstrap <file.ub>")
        sys.exit(1)
    }
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
