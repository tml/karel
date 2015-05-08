package detective

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

import Knowledge._
import parsing._

class Detective(semantics: KarelSemantics) {

  private val visited = new HashSet[Def]
  private val postcondition = new HashMap[Def, Knowledge]

  private var hints = new ArrayBuffer[Diagnostic]

  private def warnAbout(msg: String, node: Node) {
    hints += Diagnostic(node.pos, msg, 2)
  }

  private def stopWithBug(msg: String, node: Node): Knowledge = {
    hints += new Diagnostic(node.pos, msg, 1)
    CONTRADICTION
  }

  val result: Seq[Diagnostic] = {
    semantics.defs.values.foreach(check)
    hints.sortWith(_.pos < _.pos)
  }

  private def check(deF: Def): Knowledge = postcondition.getOrElseUpdate(deF, {
    if (visited.add(deF)) {
      check(deF.body, TAUTOLOGY)
    } else {
      // TODO How do we deal with recursive calls?
      TAUTOLOGY
    }
  })

  private def check(block: Block, initial: Knowledge): Knowledge = {
    var knowledge = initial
    for (statement <- block.statements) {
      if (knowledge == CONTRADICTION) {
        warnAbout("dead code", statement)
        return CONTRADICTION
      }
      knowledge = check(statement, knowledge)
    }
    knowledge
  }

  private def check(statement: Statement, above: Knowledge): Knowledge = statement match {

    case x @ Call("moveForward") =>
      if (above implies FRONT_IS_BLOCKED) stopWithBug("cannot move through wall", x)
      else above.moveForward

    case Call("turnLeft") =>
      above.turnLeft
    case Call("turnAround") =>
      above.turnAround
    case Call("turnRight") =>
      above.turnRight

    case x @ Call("pickBeeper") =>
      if (above implies NO_BEEPER) stopWithBug("there is no beeper to pick", x)
      else above.pickBeeper

    case x @ Call("dropBeeper") =>
      if (above implies ON_BEEPER) stopWithBug("cannot drop another beeper", x)
      else above.dropBeeper

    // TODO How do we deal with preconditions?
    // Should we simply check each function again for each client?
    // That would probably require a static "stack trace" to be readable.
    // TODO Recursive functions yield TAUTOLOGY as of now;
    // Can we do better for tail-recursive functions?
    case Call(target) => check(semantics.defs(target))

    case x @ IfThen(condition, th3n) =>
      val p = learn(condition)
      if (above implies p) {
        warnAbout("condition is always true", x)
        check(th3n, above)
      } else if (above implies !p) {
        warnAbout("condition is always false", x)
        above
      } else {
        check(th3n, above && p) || (above && !p)
      }

    case x @ IfThenElse(condition, th3n, e1se) =>
      val p = learn(condition)
      if (above implies p) {
        warnAbout("condition is always true", x)
        check(th3n, above)
      } else if (above implies !p) {
        warnAbout("condition is always false", x)
        check(e1se, above)
      } else {
        check(th3n, above && p) || check(e1se, above && !p)
      }

    case x @ While(condition, body) =>
      val p = learn(condition)
      if (above implies !p) {
        warnAbout("loop is never entered", x)
        above
      } else if (check(body, p) implies p) {
        stopWithBug("infinite loop", x)
      } else {
        !p
      }

    case Repeat(times, body) =>
      Stream.iterate(above)(check(body, _)).drop(times).head
  }

  private def learn(condition: Condition): Knowledge = condition match {

    case False() => CONTRADICTION
    case True() => TAUTOLOGY

    case OnBeeper() => ON_BEEPER
    case BeeperAhead() => BEEPER_AHEAD
    case FrontIsClear() => FRONT_IS_CLEAR
    case LeftIsClear() => LEFT_IS_CLEAR
    case RightIsClear() => RIGHT_IS_CLEAR

    case Not(_, p) => !learn(p)
    case Conjunction(p, _, q) => learn(p) && learn(q)
    case Disjunction(p, _, q) => learn(p) || learn(q)
    case Parenthesized(p) => learn(p)
  }
}
