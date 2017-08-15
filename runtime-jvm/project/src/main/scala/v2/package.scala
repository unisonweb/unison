package org.unisonweb.codegeneration

package object v2 {
  def applySignature(i: Int): String =
    "def apply(rec: Lambda, " + (0 until i).commas(i => s"x$i: D, x${i}b: V") + commaIf(i) + "r: R): D"

  def applyNSignature: String =
    "def apply(rec: Lambda, xs: Array[Slot], r: R): D"
}
