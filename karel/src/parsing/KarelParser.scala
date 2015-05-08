package parsing

object KarelParser {
  private val COMMENT = "(?ms)//.*?$|/\\*.*?\\*/".r
  private val HORIZONTAL = ".".r

  private def stripComments(input: String): String = {
    COMMENT.replaceAllIn(input, m => {
      val comment = m.matched
      if (comment.charAt(1) == '/') "" else HORIZONTAL.replaceAllIn(comment, _ => " ")
    })
  }

  def syntacticParse(input: String): Either[Diagnostic, Program] = {
    KarelSyntax.parse(stripComments(input)) match {
      case KarelSyntax.NoSuccess(msg, next) => Left(Diagnostic(next.pos, msg))
      case KarelSyntax.Success(program, _) => Right(program)
    }
  }
}
