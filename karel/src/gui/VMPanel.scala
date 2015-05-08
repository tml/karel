package gui

import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.ScrollPane
import scala.swing.Swing

import vm.Instruction

class VMPanel(font: java.awt.Font) extends BoxPanel(Orientation.Horizontal) {

  val table = new BytecodeTable(font)
  val stack = new StackTable(font)

  contents += new ScrollPane(table)
  contents += new BoxPanel(Orientation.Vertical) {
    contents += Swing.VGlue
    contents += stack
  }

  visible = false

  def setProgram(program: Seq[Instruction]) {
    table.set(program)
  }

  def clearStack() {
    stack.set(Nil)
  }

  def update(pc: Int, stk: List[Int]) {
    table.highlightLine(pc)
    stack.set(stk)
  }
}
