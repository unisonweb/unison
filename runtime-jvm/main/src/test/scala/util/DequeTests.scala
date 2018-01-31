package org.unisonweb.util

import org.unisonweb.EasyTest._

object DequeTests {

  val D = Deque

  lazy val tests = suite("Deque")(
    test("basic examples") { implicit T =>
      expect { D(1) == D(1) }
      expect { D(1).toVector == Vector(1) }
      expect { D(1,2) ++ D(3) == D(1,2,3) }
    },
    test("reflexive") { implicit T =>
      deques.foreach { d =>
        expect1 (d == d)
        expect1 (d.toVector.size == d.size)
      }
      ok
    },
    test("++") { implicit T =>
      deques.zip(deques).foreach { case (d1,d2) =>
        expect1 { (d1 ++ d2).toVector == d1.toVector ++ d2.toVector }
      }
      ok
    },
    test("take/drop/reverse") { implicit T =>
      deques.foreach { d =>
        equal1(d.reverse.toVector, d.toVector.reverse)
        val n = intIn(-3, d.size + 3)
        val m = intIn(-3, d.size + 3)
        equal1(d.take(n).toVector, d.toVector.take(n))
        equal1(d.take(m).toVector, d.toVector.take(m))
        equal1(d.drop(n).toVector, d.toVector.drop(n))
        equal1(d.drop(m).toVector, d.toVector.drop(m))
      }
      ok
    }
  )

  def deques(implicit T: Env) =
    (Vector(0,1,2,3,4) ++ intsIn(95)(5,4096)).map(deque(_))

  def deque(size: Int)(implicit T: Env): Deque[Int] = {
    val split = intIn(0, size)
    val buf = D.empty[Int]
    val left = intsIn(split)(0, 100)
    val right = intsIn(size - split)(0, 100)
    val buf2 = right.foldLeft(buf)((buf,i) => buf :+ i)
    left.foldLeft(buf2)((buf,i) => i +: buf)
  }
}
