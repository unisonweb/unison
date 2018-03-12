package org.unisonweb.util

import org.unisonweb.EasyTest._

object SequenceTests {

  val S = Sequence

  val tests = suite("Sequence")(
    test("basic examples") { implicit T =>
      expect { S(1) == S(1) }
      expect { S(1) ++ S(1) == S(1,1) }
      expect { S(1,2,3,4) ++ S(1) == S(1,2,3,4,1) }
    },
    test("reflexive") { implicit T =>
      (0 until 100).foreach { _ =>
        val s = seq.run
        expect1 (s == s)
      }
      ok
    },
    test("++") { implicit T =>
      (0 until 100).foreach { _ =>
        val s1 = seq.run
        val s2 = seq.run
        val s3 = seq.run
        (s1 ++ s2) ++ s3 === s1.toList ++ s2.toList ++ s3.toList
        s1 ++ (s2 ++ s3) === s1.toList ++ s2.toList ++ s3.toList
      }
      ok
    },
    test("take/drop/reverse/map/foldLeft") { implicit T =>
      (0 until 1000).foreach { size =>
        val s = seqOf(size).run
        val n = longIn(-3, s.size + 6)
        expect1(s.take(n) === s.toList.take(n.toInt))
        expect1((s.take(n) ++ s.drop(n)) == s)
        expect1(s.drop(n) === s.toList.drop(n.toInt))
        expect1(s.map(x => x) == s)
        expect1(s.reverse === s.toList.reverse)
        expect1(s.foldLeft(0)(_ - _) == s.toList.foldLeft(0)(_ - _))
      }
      ok
    }
  )

  implicit class SeqOps[A](s: Sequence[A]) {
    def ===(e: List[A]) = (0L until s.size).map(s(_)).toList == e.toList
  }

  def seqOf(size: Int): Test[Sequence[Int]] = test { implicit T =>
    if (size <= 0) S.empty
    else intIn(0,5) match {
      case 0 => S((0 until size): _*)
      case 1 => (0 until size).foldLeft(S.empty[Int])((buf,i) => i +: buf)
      case 2 => seqOf(size/4).run ++ seqOf(size/4).run ++ seqOf(size/4).run ++ seqOf(size/4).run
      case 3 =>
        seqOf(size/2).run ++ seqOf(size/4).run ++ seqOf(size/8).run ++ seqOf(size/16).run
      case 4 =>
        seqOf(size/16).run ++ seqOf(size/4).run ++ seqOf(size/8).run ++ seqOf(size/2).run
    }
  }

  def seq: Test[Sequence[Int]] = test { implicit T => seqOf(intIn(0, 4096)).run }
}
