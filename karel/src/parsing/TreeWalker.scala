package parsing

class TreeWalker(enter: PartialFunction[Node, Unit]) {

  val Enter = enter.lift

  def walk(node: Node) {
    Enter(node)
    node match {
      case Program(commands) =>
        commands.foreach(walk)

      case Def(_, block) =>
        walk(block)

      case Block(statements, _) =>
        statements.foreach(walk)

      case IfThen(_, th3n) =>
        walk(th3n)

      case IfThenElse(_, th3n, e1se) =>
        walk(th3n)
        walk(e1se)

      case While(_, block) =>
        walk(block)

      case Repeat(_, block) =>
        walk(block)

      case _ =>
    }
  }
}
