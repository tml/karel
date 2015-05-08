package parsing

import scala.util.parsing.input.Position

case class Diagnostic(pos: Position, msg: String, severity: Int = 0)
