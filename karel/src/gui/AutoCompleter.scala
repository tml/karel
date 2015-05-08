package gui

object AutoCompleter {
  val identifier = """\p{javaJavaIdentifierStart}\p{javaJavaIdentifierPart}*\(\)""".r
  val reifitnedi = """\p{javaJavaIdentifierPart}*\p{javaJavaIdentifierStart}""".r
}

class AutoCompleter(text: String, lineUntilCursor: String) {

  lazy val result: Either[List[String], String] = {
    val suffixes = fittingSuffixes
    if (suffixes.isEmpty) {
      Right("")
    } else {
      val lcp = longestCommonPrefix(suffixes)
      if (lcp.isEmpty()) {
        Left(suffixes)
      } else {
        Right(lcp)
      }
    }
  }

  private def fittingSuffixes = {
    val prefix = currentPrefix
    val prefixLength = prefix.length
    val fittingNames = allNames.filter(name => (name.length > prefixLength) && name.startsWith(prefix))
    fittingNames.map(_.substring(prefixLength))
  }

  private def currentPrefix = {
    AutoCompleter.reifitnedi.findFirstIn(lineUntilCursor.reverse).getOrElse("").reverse
  }

  private def allNames = {
    AutoCompleter.identifier.findAllIn(text).toSet.toList.sorted
  }

  private def longestCommonPrefix(words: Traversable[String]): String = {
    words.foldLeft(words.head)(longestCommonPrefix)
  }

  // TODO Is there an elegant AND EFFICIENT functional solution?
  private def longestCommonPrefix(a: String, b: String): String = {
    val n = a.length.min(b.length)
    var i = 0
    while ((i < n) && (a.charAt(i) == b.charAt(i))) {
      i += 1
    }
    a.substring(0, i)
  }
}
