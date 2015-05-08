package vm

import java.util.concurrent.atomic.AtomicReference

import Instruction._
import logic.Karel

case class IllegalInstruction(bytecode: Int) extends Exception

object VM {
  final val TIMEOUT = 10000000000L
}

class VM(val program: Seq[Instruction], atomicKarel: AtomicReference[Karel], onCall: (Int, Int) => Unit, onReturn: => Unit, onInfiniteLoop: => Unit) {

  private var pc = 256

  def PC = pc

  def currentInstruction = program(pc)

  private var stack = List.empty[Int]

  private var callDepth = 0

  def getStack = stack

  private def push(x: Int) {
    stack = x :: stack
  }

  private def pop(): Int = {
    val result = stack.head
    stack = stack.tail
    return result
  }

  def stepInto(vmVisible: Boolean) {
    executeUnpausedInstructions(vmVisible)
    executeOneInstruction()
    executeUnpausedInstructions(vmVisible)
  }

  def executeUnpausedInstructions(vmVisible: Boolean) {
    while (!currentInstruction.shouldPause(vmVisible)) {
      executeOneInstruction()
    }
  }

  def stepOver() {
    if (currentInstruction.category == TAIL) {
      // Tail calls don't grow the stack, so a normal "step over"
      // would resemble a "step into" instead.
      stepUntil(callDepth - 1)
    } else {
      stepUntil(callDepth)
    }
  }

  def stepReturn() {
    stepUntil(callDepth - 1)
  }

  private def stepUntil(targetDepth: Int) {
    val start = System.nanoTime()
    stepInto(false)
    while ((callDepth > targetDepth) && (System.nanoTime() - start < VM.TIMEOUT)) {
      executeOneInstruction()
    }
    if (callDepth > targetDepth) {
      onInfiniteLoop
    }
  }

  def executeOneInstruction() {
    val instruction = currentInstruction

    val cat = instruction.category
    cat match {
      case NORM =>
        executeBasicInstruction(instruction.bytecode)

      case PUSH =>
        push(instruction.target)
        pc += 1

      case LOOP =>
        executeLoop(instruction)

      case CALL =>
        // When tail calls are enabled, this will find the first exit
        val leave = program.view.drop(instruction.target).find(_.leaves)
        onCall(instruction.line, leave.getOrElse(program(instruction.target)).line)
        push(pc)
        pc = instruction.target
        callDepth += 1

      case JUMP => pc = instruction.target

      case J0MP =>
        pc = (if (pop() == 0) instruction.target else pc + 1)

      case J1MP =>
        pc = (if (pop() != 0) instruction.target else pc + 1)

      case TAIL => pc = instruction.target

      case illegal =>
        throw new AssertionError("illegal instruction category " + cat.toHexString)
    }
  }

  def executeBasicInstruction(bytecode: Int) {
    bytecode match {
      case RETURN =>
        onReturn
        callDepth -= 1
        pc = pop()

      case MOVE_FORWARD =>
        execute(_.moveForward)

      case TURN_LEFT =>
        execute(_.turnLeft)

      case TURN_AROUND =>
        execute(_.turnAround)

      case TURN_RIGHT =>
        execute(_.turnRight)

      case PICK_BEEPER =>
        execute(_.pickBeeper)

      case DROP_BEEPER =>
        execute(_.dropBeeper)

      case ON_BEEPER =>
        query(_.onBeeper)

      case BEEPER_AHEAD =>
        query(_.beeperAhead)

      case LEFT_IS_CLEAR =>
        query(_.leftIsClear)

      case FRONT_IS_CLEAR =>
        query(_.frontIsClear)

      case RIGHT_IS_CLEAR =>
        query(_.rightIsClear)

      case NOT =>
        push(if (pop() == 0) 1 else 0)

      case AND =>
        binaryOperation(_ & _)

      case OR =>
        binaryOperation(_ | _)

      case XOR =>
        binaryOperation(_ ^ _)

      case _ =>
        throw IllegalInstruction(bytecode)
    }
    pc += 1
  }

  def execute(f: Karel => Karel) {
    atomicKarel.set(f(karel))
  }

  def query(f: Karel => Boolean) {
    push(if (f(karel)) 1 else 0)
  }

  def binaryOperation(f: (Int, Int) => Int) {
    push(f(pop(), pop()))
  }

  def executeLoop(instruction: Instruction) {
    val x = pop() - 1
    if (x > 0) {
      push(x)
      pc = instruction.target
    } else {
      pc += 1
    }
  }

  private def karel = atomicKarel.get()
}
