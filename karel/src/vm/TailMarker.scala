package vm

import parsing._
import Instruction.builtinCommands

object TailMarker {

  def mark(defs: Traversable[Def]) {
    defs.foreach(mark)
  }

  private def mark(deF: Def) {
    mark(deF.body)
    deF.isTail = deF.body.isTail
  }

  private def mark(body: Block) {
    if (!body.statements.isEmpty) {
      val statement = body.statements.last
      mark(statement)
      body.isTail = statement.isTail
    }
  }

  private def mark(statement: Statement) {
    statement match {
      case c @ Call(identifier) =>
        c.isTail = !builtinCommands.contains(identifier)

      case IfThen(_, th3n) =>
        mark(th3n)
      // IfThen is never tail, because the condition might be false :)

      case IfThenElse(_, th3n, e1se) =>
        mark(th3n)
        mark(e1se)
        statement.isTail = th3n.isTail && e1se.isTail

      case _ =>
    }
  }
}
