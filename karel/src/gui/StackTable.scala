package gui

import java.awt.Dimension

import scala.swing.Table

import javax.swing.JTable
import javax.swing.table.AbstractTableModel

class StackTable(font: java.awt.Font) extends Table {

  model = new StackTableModel
  peer.setAutoResizeMode(JTable.AUTO_RESIZE_OFF)
  peer.setFont(font)

  val columnModel = peer.getColumnModel()

  val column = columnModel.getColumn(0)
  val renderer = peer.getDefaultRenderer(model.getColumnClass(0))
  val comp = renderer.getTableCellRendererComponent(peer, "stack", false, false, 0, 0)
  column.setPreferredWidth(comp.getPreferredSize().width)
  column.setResizable(false)

  val perfectWidth = columnModel.getColumn(0).getPreferredWidth()
  preferredViewportSize = new Dimension(perfectWidth, 1)
  peer.setFillsViewportHeight(true)

  def set(newStack: List[Int]) {
    model.asInstanceOf[StackTableModel].set(newStack)
  }

  def highlightLine(line: Int) {
    peer.setRowSelectionInterval(line, line)
  }
}

class StackTableModel(var stack: List[Int] = Nil) extends AbstractTableModel {

  def set(newStack: List[Int]) {
    if (newStack ne stack) {
      stack = newStack
      fireTableDataChanged()
    }
  }

  def getRowCount: Int = stack.length

  def getColumnCount: Int = 1

  def getValueAt(row: Int, column: Int): Object = {
    " %3x".format(stack(row))
  }
}
