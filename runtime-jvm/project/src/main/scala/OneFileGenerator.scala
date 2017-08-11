package org.unisonweb.codegeneration

import java.io.File

abstract class OneFileGenerator(filename: String) {
  def source: String
  val N = maxInlineArity
  def apply(outDir: File): (File, String) = (new File(outDir, "Arity.scala"), source)
}
