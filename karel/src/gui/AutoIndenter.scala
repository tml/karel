package gui

object AutoIndenter {

  def indent(program: String): String = {
    val lines = program.lines.map(dropSpaces).toArray
    val indentation = new Array[Int](lines.length)
    var reference = 0
    for (i <- 0 until lines.length) {
      val braces = lines(i).filter(isBrace)
      val leadingClosers = braces.prefixLength(closingBrace)
      indentation(i) = reference - leadingClosers
      reference = reference + braces.length - braces.count(closingBrace) * 2
    }
    val minimum = indentation.min
    val zipped = indentation.map(_ - minimum).zip(lines)
    zipped.map(generateSpaces).mkString("\n")
  }

  private def dropSpaces(line: String): String = line.dropWhile(_ == ' ')

  private def isBrace(c: Char) = (c == '{') || (c == '}')

  private def closingBrace(c: Char) = (c == '}')

  private def generateSpaces(ns: (Int, String)): String = {
    val (n, s) = ns
    "    " * n + (if (s.startsWith("*")) " " else "") + s
  }
}
