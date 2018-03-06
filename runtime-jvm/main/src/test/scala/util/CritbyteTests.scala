package org.unisonweb.util

import org.unisonweb.EasyTest._

object CritbyteTests {

  val tests = suite("Critbyte")(
    test("insert (duplicate key)") { implicit T =>
      0 until 10 foreach { size =>
        val key = genBytes(intIn(0,size))
        val cb = Critbyte.empty[Int].insert(key, 23)
                                    .insert(key, 24)
        equal1(cb.lookup(key), Some(24))
      }
      ok
    },
    test("works like a map") { implicit T =>
      0 until 100 foreach { i =>
        val ts = map(intIn(0,i), genBytes(intIn(0,4)), int)
        note("-----------", false)
        ts.foldLeft((Critbyte.empty[Int],Map[Bytes.Seq,Int]())) {
          case ((cb, m), (k, v)) =>
            val ncb = cb.insert(k, v)
            val nm = m + (k -> v)
            note("insert: " + (k -> v), false)
            note("cb (before): " + cb, false)
            note("cb (after): " + ncb, false)
            equal1(ncb.lookup(k), nm.get(k))
            (ncb, nm)
        }
      }
      ok
    },
    test("prefixedBy.identity") { implicit T =>
      0 until 25 foreach { i =>
        val cb = genCritbytes(i, int)
        expect1 { cb.prefixedBy(Bytes.Seq.empty) eq cb }
      }
      ok
    }
  )

  def genBytes(size: => Int)(implicit T: Env): Bytes.Seq =
    Bytes.Seq(replicate(size)(byte))

  def genCritbytes[A](size: => Int, a: => A)(implicit T: Env): Critbyte[A] =
    map(size, genBytes(intIn(0,256)), a).foldLeft(Critbyte.empty[A])((cb, kv) => cb insert (kv._1, kv._2))
}
