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
        def smallKey = genBytes(intIn(0,4))
        val ts = map(intIn(0,i), smallKey, int)
        note("-----------", false)
        val (cb, m) = ts.foldLeft((Critbyte.empty[Int],Map[Bytes.Seq,Int]())) {
          case ((cb, m), (k, v)) =>
            val ncb = cb.insert(k, v)
            val nm = m + (k -> v)
            note("insert: " + (k -> v), false)
            note("cb (before): " + cb, false)
            note("cb (after): " + ncb, false)
            expect1 { ncb.remove(k).lookup(k) == None }
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
    },
    test("prefixedBy.homomorphism") { implicit T =>
      0 until 25 foreach { i =>
        val cb = genCritbytes(i, int)
        val keys = cb.keys
        val k = keys.drop(intIn(0,i)).headOption.getOrElse(Bytes.Seq.empty)
        val prefix = k.take(intIn(0, k.size))
        expect1 (keys.filter(prefix isPrefixOf _) == cb.prefixedBy(prefix).keys)
      }
      ok
    },

    test("prefixedBy.prefix") { implicit T =>
      0 until 25 foreach { i =>
        val cb = genCritbytes(i, int)
        expect1(cb.prefixedBy(cb.prefix) == cb)
      }
      ok
    },

    test("union") { implicit T =>
      0 until 250 foreach { i =>
        val x = genCritbytes(intIn(0, i), 'x)
        val y = genCritbytes(intIn(0, i), 'y)
        val u = x union y
        note("x: " + x.toString)
        note("y: " + y.toString)
        note("u: " + u.toString)
        equal1(u.keys.toSet, (x.keys union y.keys).toSet)
        u.keys.foreach { k =>
          equal1(u.lookup(k), y.lookup(k) orElse x.lookup(k))
        }
      }
      ok
    },

    test("union.associative") { implicit T =>
      0 until 250 foreach { i =>
        val x = genCritbytes(intIn(0, i), 'x)
        val y = genCritbytes(intIn(0, i), 'y)
        val z = genCritbytes(intIn(0, i), 'z)

        note("-----x-----")
        note(x.toString)
        note("-----y-----")
        note(y.toString)
        note("-----z-----")
        note(z.toString)

        equal1(((x union y) union z).toString,
               (x union (y union z)).toString)
      }
      ok
    },

    test("unionWith.commutative") { implicit T =>
      0 until 250 foreach { i =>
        val x = genCritbytes(intIn(0, i), int)
        val y = genCritbytes(intIn(0, i), int)

        equal1(x.unionWith(y)(_ + _).toString, y.unionWith(x)(_ + _).toString)
      }
      ok
    },

    test("unionWith.identity") { implicit T =>
      0 until 25 foreach { i =>
        val x = genCritbytes(intIn(0, i), int)
        equal1(x union Critbyte.empty, x)
        equal1(Critbyte.empty union x, x)
      }
      ok
    }
  )

  def genBytes(size: => Int)(implicit T: Env): Bytes.Seq =
    Bytes.Seq(replicate(size)(byte))

  def genCritbytes[A](size: => Int, a: => A)(implicit T: Env): Critbyte[A] =
    map(size, genBytes(intIn(0,3)), a).foldLeft(Critbyte.empty[A])((cb, kv) => cb insert (kv._1, kv._2))

}
