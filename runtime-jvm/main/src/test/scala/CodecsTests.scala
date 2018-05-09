package org.unisonweb

import org.unisonweb.EasyTest._
//import org.unisonweb.util.PrettyPrint
// import Term.Syntax._
import Term.Term
import BuiltinTypes._

object CodecsTests {
  val env0 = Environment.standard

  def roundTrip(t: Term) = {
    Codecs.decodeTerm(Codecs.encodeTerm(t))
  }

  def roundTrip(p: Value) = {
    val bytes = Codecs.encodeValue(p)
    // println(bytes.toList.flatten)
    Codecs.decodeValue(bytes)
  }

  val tests = suite("codecs") (
    test("huge tuple") { implicit T =>
      roundTrip(Tuple.value(
        (0 until 10000 map { i => Value.Unboxed(i, UnboxedType.Int64) }): _*))
      ok
    }
  )
}
