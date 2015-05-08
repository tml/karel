package vm

import scala.collection.mutable.HashMap

import Instruction._
import parsing._
import util.IdentityGenerator

class CodeGenerator(semantics: KarelSemantics, optimizeTailCalls: Boolean) {

  val program = Instruction.dummyBuffer()

  def pc = program.length

  def lastInstruction = program.last

  def removeLastInstruction() {
    program.reduceToSize(program.length - 1)
  }

  def generateInstruction(bytecode: Int, node: Node) {
    program.append(Instruction(bytecode, node.pos))
  }

  val id = new IdentityGenerator
  val start = new HashMap[Int, Int]

  def translateCalls() {
    program.view.filter(_.isCall).transform(_.translateTarget(start))
  }

  val result = {
    if (optimizeTailCalls) {
      TailMarker.mark(semantics.reachableDefs)
    }
    semantics.reachableDefs.foreach(generate)
    translateCalls()
    program.toSeq
  }

  def generate(deF: Def) {
    val i = id(deF.identifier)
    //    if (lastInstruction.target == i) {
    //      println("TAIL FALL")
    //      removeLastInstruction()
    //    }
    start.put(i, pc)
    generate(deF.body)
    if (deF.returns) {
      generateInstruction(RETURN, deF.body.closingBrace)
    }
  }

  def generate(block: Block) {
    block.statements.foreach(generate)
  }

  def removeLastNegation(): Int = {
    if (lastInstruction.bytecode == NOT) {
      removeLastInstruction()
      J0MP ^ J1MP // dude...
    } else 0
  }

  def prepareForwardJump(category: Int, node: Node) = {
    generateInstruction(category ^ removeLastNegation(), node)
    pc - 1
  }

  def patchForwardJump(where: Int) {
    program(where) = program(where).withTarget(pc)
  }

  def generate(statement: Statement) {
    statement match {

      case x @ IfThen(condition, th3n) =>
        generate(condition)
        val over = prepareForwardJump(J0MP, x)
        generate(th3n)
        patchForwardJump(over)

      case x @ IfThenElse(condition, th3n, e1se) =>
        generate(condition)
        val overThen = prepareForwardJump(J0MP, x)
        generate(th3n)
        if (th3n.returns) {
          val overElse = prepareForwardJump(JUMP, th3n.closingBrace)
          patchForwardJump(overThen)
          generate(e1se)
          patchForwardJump(overElse)
        } else {
          patchForwardJump(overThen)
          generate(e1se)
        }

      case x @ While(condition, block) =>
        val back = pc
        generate(condition)
        val over = prepareForwardJump(J0MP, x)
        generate(block)
        generateInstruction(JUMP | back, block.closingBrace)
        patchForwardJump(over)

      case x @ Repeat(times, body) =>
        generateInstruction(PUSH | times, x)
        val back = pc
        generate(body)
        generateInstruction(LOOP | back, body.closingBrace)

      case x @ Call(target) =>
        builtinCommands.get(target) match {
          case Some(bytecode) => generateInstruction(bytecode, x)
          case None =>
            val category = if (x.isTail) TAIL else CALL
            generateInstruction(category | id(target), x)
        }
    }
  }

  def generate(condition: Condition) {
    condition match {
      case x: False => generateInstruction(PUSH | 0, x)
      case x: True => generateInstruction(PUSH | 1, x)

      case x: OnBeeper => generateInstruction(ON_BEEPER, x)
      case x: BeeperAhead => generateInstruction(BEEPER_AHEAD, x)
      case x: LeftIsClear => generateInstruction(LEFT_IS_CLEAR, x)
      case x: FrontIsClear => generateInstruction(FRONT_IS_CLEAR, x)
      case x: RightIsClear => generateInstruction(RIGHT_IS_CLEAR, x)

      case Not(not, p) =>
        generate(p)
        generateInstruction(NOT, not)

      case Conjunction(p, and, q) =>
        generate(p)
        generate(q)
        generateInstruction(AND, and)

      case Disjunction(p, or, q) =>
        generate(p)
        generate(q)
        generateInstruction(OR, or)

      case Parenthesized(condition) =>
        generate(condition)
    }
  }
}
