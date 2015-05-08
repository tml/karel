package gui

import java.awt.Dimension

import scala.swing.Table

import javax.swing.JTable
import javax.swing.table.AbstractTableModel
import vm.Instruction

class BytecodeTable(font: java.awt.Font) extends Table {

  model = new BytecodeTableModel
  peer.setAutoResizeMode(JTable.AUTO_RESIZE_OFF)
  peer.setFont(font)

  val columnModel = peer.getColumnModel()
  val exampleRow = Array("000 ", "8000 ", "JUMP 000 ")

  for (i <- 0 to 2) {
    // see http://docs.oracle.com/javase/tutorial/displayCode.html?code=http://docs.oracle.com/javase/tutorial/uiswing/examples/components/TableRenderDemoProject/src/components/TableRenderDemo.java
    val column = columnModel.getColumn(i)
    val renderer = peer.getDefaultRenderer(model.getColumnClass(i))
    val comp = renderer.getTableCellRendererComponent(peer, exampleRow(i), false, false, 0, i)
    column.setPreferredWidth(comp.getPreferredSize().width)
    column.setResizable(false)
  }

  val perfectWidth = (0 to 2).map(columnModel.getColumn(_).getPreferredWidth()).sum
  preferredViewportSize = new Dimension(perfectWidth, 1)
  peer.setFillsViewportHeight(true)

  def set(newProgram: Seq[Instruction]) {
    model.asInstanceOf[BytecodeTableModel].set(newProgram)
  }

  def highlightLine(line: Int) {
    val row = line - 256
    peer.setRowSelectionInterval(row, row)
    scrollToRow(row + 3)
    scrollToRow(row)
  }

  private def scrollToRow(row: Int) {
    val rect = peer.getCellRect(row, 0, true)
    peer.scrollRectToVisible(rect)
  }
}

class BytecodeTableModel(var program: Seq[Instruction] = Seq.empty) extends AbstractTableModel {

  def set(newProgram: Seq[Instruction]) {
    program = newProgram
    fireTableDataChanged()
  }

  def getRowCount: Int = program.length - 256 max 0

  def getColumnCount: Int = 3

  def getValueAt(row: Int, column: Int): Object = {
    val line = row + 256
    val instruction = program(line)
    column match {
      case 0 => "%3x".format(line)
      case 1 => "%4x".format(instruction.bytecode)
      case 2 => instruction.mnemonic
    }
  }

  override def getColumnName(column: Int) = columnNames(column)

  val columnNames = Array[String]("@", "code", "mnemonic")
}
