package parsing

import scala.util.parsing.input.Positional

sealed abstract class Node extends Positional {
  var isTail = false
  def returns = !isTail
}

case class Program(defs: List[Def]) extends Node

case class Def(identifier: String, body: Block) extends Node

// note: In Karel's language, a block is by design NOT a statement!
// That is, you cannot arbitrarily place blocks where statements are required.
// One the other hand, blocks are required for if and while statements.
// We hope this language choice will encourage a good programming style.
case class Block(statements: List[Statement], closingBrace: ClosingBrace) extends Node

case class ClosingBrace() extends Node

sealed abstract class Statement extends Node

case class IfThen(condition: Condition, th3n: Block) extends Statement

case class IfThenElse(condition: Condition, th3n: Block, e1se: Block) extends Statement

case class While(condition: Condition, body: Block) extends Statement

case class Repeat(times: Int, body: Block) extends Statement

case class Call(identifier: String) extends Statement

sealed abstract class Condition extends Node

case class False() extends Condition

case class True() extends Condition

case class OnBeeper() extends Condition

case class BeeperAhead() extends Condition

case class FrontIsClear() extends Condition

case class LeftIsClear() extends Condition

case class RightIsClear() extends Condition

case class Operator(token: String) extends Node

case class Not(not: Operator, p: Condition) extends Condition

case class Conjunction(p: Condition, and: Operator, q: Condition) extends Condition

case class Disjunction(p: Condition, or: Operator, q: Condition) extends Condition

case class Parenthesized(condition: Condition) extends Condition
