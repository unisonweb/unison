package org.unisonweb.codegeneration

import java.io.File

abstract class OneFileGenerator(filename: String) {
  def source: String
  def apply(outDir: File): (File, String) = (new File(outDir, filename), source)
}
