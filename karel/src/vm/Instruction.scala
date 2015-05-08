package vm

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.Position

case class Instruction(val underlying: Long) extends AnyVal {
  import Instruction._

  def line = (underlying >>> 32).toInt
  def column = underlying.toInt >> 16

  def bytecode = underlying.toInt & 0xffff
  def category = underlying.toInt & 0xf000
  def target = underlying.toInt & 0x0fff

  def withBytecode(bytecode: Int) = Instruction(underlying & ~0xffff | bytecode)
  def withCategory(category: Int) = Instruction(underlying & ~0xf000 | category)
  def withTarget(target: Int) = Instruction(((underlying)) & ~0x0fff | target)
  def translateTarget(start: Function[Int, Int]) = withTarget(start(target))

  def hasVisibleEffect = (bytecode >= MOVE_FORWARD) && (bytecode <= DROP_BEEPER)

  def shouldPauseInSourceCode = (category != NORM || bytecode == RETURN) && (category != JUMP)

  def shouldPause(vmVisible: Boolean) = {
    hasVisibleEffect || ((line > 0) && (vmVisible || shouldPauseInSourceCode))
  }

  def leaves = (bytecode == RETURN) || (category == TAIL)

  def isCall = (category == CALL) || (category == TAIL)

  def mnemonic: String = {
    category match {
      case PUSH => "PUSH %03x".format(target)
      case LOOP => "LOOP %03x".format(target)
      case CALL => "CALL %03x".format(target)
      case JUMP => "JUMP %03x".format(target)
      case J0MP => "J0MP %03x".format(target)
      case J1MP => "J1MP %03x".format(target)
      case TAIL => "TAIL %03x".format(target)
      case _ => bytecode match {
        case RETURN => "RET"
        case MOVE_FORWARD => "MOVE"
        case TURN_LEFT => "TRNL"
        case TURN_AROUND => "TRNA"
        case TURN_RIGHT => "TRNR"
        case PICK_BEEPER => "PICK"
        case DROP_BEEPER => "DROP"
        case ON_BEEPER => "BEEP"
        case BEEPER_AHEAD => "HEAD"
        case LEFT_IS_CLEAR => "LCLR"
        case FRONT_IS_CLEAR => "FCLR"
        case RIGHT_IS_CLEAR => "RCLR"
        case NOT => "NOT"
        case AND => "AND"
        case OR => "OR"
        case XOR => "XOR"
      }
    }
  }
}

object Instruction {
  def apply(bytecode: Int): Instruction = new Instruction(bytecode & 0xffff)
  def apply(bytecode: Int, line: Int, column: Int): Instruction = new Instruction(line.toLong << 32 | (column & 0xffff) << 16 | bytecode & 0xffff)
  def apply(bytecode: Int, pos: Position): Instruction = Instruction(bytecode, pos.line, pos.column)

  val RETURN = 0x0

  val MOVE_FORWARD = 0x1
  val TURN_LEFT = 0x2
  val TURN_AROUND = 0x3
  val TURN_RIGHT = 0x4
  val PICK_BEEPER = 0x5
  val DROP_BEEPER = 0x6

  val ON_BEEPER = 0x7
  val BEEPER_AHEAD = 0x8
  val LEFT_IS_CLEAR = 0x9
  val FRONT_IS_CLEAR = 0xa
  val RIGHT_IS_CLEAR = 0xb

  val NOT = 0xc
  val AND = 0xd
  val OR = 0xe
  val XOR = 0xf

  val NORM = 0x0000

  val PUSH = 0x8000
  val LOOP = 0x9000
  val CALL = 0xa000
  val JUMP = 0xb000

  val J0MP = 0xc000
  val J1MP = 0xd000

  val TAIL = 0xe000

  val builtinCommands = Map(
    "moveForward" -> MOVE_FORWARD,
    "turnLeft" -> TURN_LEFT,
    "turnAround" -> TURN_AROUND,
    "turnRight" -> TURN_RIGHT,
    "pickBeeper" -> PICK_BEEPER,
    "dropBeeper" -> DROP_BEEPER)

  val dummy = Instruction(RETURN)
  def dummyBuffer() = new ArrayBuffer[Instruction].padTo(256, dummy)
}
