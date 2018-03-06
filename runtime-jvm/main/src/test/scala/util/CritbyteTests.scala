package org.unisonweb.util

import org.unisonweb.EasyTest._

object CritbyteTests {

  val tests = suite("Critbyte")(
    test("works like a map") { implicit T =>
      0 until 100 foreach { i =>
        val ts = map(intIn(0,i), genBytes(intIn(0,256)), int)
        ts.foldLeft((Critbyte.empty[Int],Map[Bytes.Seq,Int]())) {
          case ((cb, m), (k, v)) =>
            val ncb = cb.insert(k, v)
            val nm = m + (k -> v)
            expect1(ncb.lookup(k) == nm.get(k))
            (ncb, nm)
        }
      }
      ok
    }
  )

  def genBytes(size: => Int)(implicit T: Env): Bytes.Seq =
    Bytes.Seq(replicate(size)(byte))

  //def genCritbytes[A](size: => Int, a: => A)(implicit T: Env): Critbyte[A] =
  //  map(genBytes(intIn(0,256)), a)

}
