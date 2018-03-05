package org.unisonweb.util

import org.unisonweb.EasyTest._

object TextTests {

  lazy val tests = suite("Text")(
    test("Buffer.apply") { implicit T =>
      (0 until 100) foreach { n =>
        val buf = Text.emptyBuffer
        val codepoints = replicate(n)(codepoint)
        (0 until n) foreach { i =>
          buf :+ (i, codepoints(i))
          expect1 (buf(i) == codepoints(i))
        }
      }
      ok
    },
    test("round trip (ascii)") { implicit T =>
      (0 until 100) foreach { n =>
        val s = alphas123(n)
        expect1 { Text.toString(Text.fromString(s)) == s }
      }
      ok
    },
    test("round trip (unicode)") { implicit T =>
      (0 until 100) foreach { n =>
        val s = string(n)
        val txt = Text.fromString(s)
        val ch = codepoint
        equal1 ((txt :+ ch)(txt.size), ch)
        equal1 ((ch +: txt)(0)       , ch)
        equal1 (Text.toString(Text.fromString(s)), s)
      }
      ok
    },
    test("++") { implicit T =>
      (0 until 100) foreach { n =>
        val t1 = textOf(intIn(0, n * 1)).run
        val t2 = textOf(intIn(0, n * 1)).run
        val t3 = textOf(intIn(0, n * 1)).run
        equal1 (t1 ++ (t2 ++ t3), (t1 ++ t2) ++ t3)
        equal1 (t1 ++ (t3 ++ t2), (t1 ++ t3) ++ t2)
      }
      ok
    },
    test("take/drop/reverse") { implicit T =>
      (0 until 100) foreach { n =>
        val txt = textOf(n * 100).run
        val txtList = txt.toList
        val m = intIn(-3, txt.size.toInt + 3)
        if (m >= 0 && m < txt.size) equal1 (txt(m), txtList(m))
        equal1 (txt.take(m).toList, txtList.take(m))
        equal1 (txt.drop(m).toList, txtList.drop(m))
        equal1 (txt.take(m) ++ txt.drop(m), txt)
        equal1 (txt.reverse.toList, txtList.reverse)
      }
      ok
    }
  )


  def textOf(size: Int): Test[Text.Text] = test { implicit T =>
    if (size <= 0) Text.empty
    else intIn(0,5) match {
      case 0 => (0 until size).foldLeft(Text.empty)((buf,i) => buf :+ codepoint)
      case 1 => (0 until size).foldLeft(Text.empty)((buf,i) => codepoint +: buf)
      case 2 => textOf(size/4).run ++ textOf(size/4).run ++ textOf(size/4).run ++ textOf(size/4).run
      case 3 =>
        textOf(size/2).run ++ textOf(size/4).run ++ textOf(size/8).run ++ textOf(size/16).run
      case 4 =>
        textOf(size/16).run ++ textOf(size/4).run ++ textOf(size/8).run ++ textOf(size/2).run
    }
  }
}

object RunTextTests extends App {
  run()(TextTests.tests)
}
