package gui

import scala.swing.Table
import scala.swing.event.TableRowsSelected

import javax.swing.table.AbstractTableModel
import parsing.Diagnostic

class DetectiveTable(onClick: Diagnostic => Unit) extends Table {

  model = new DetectiveTableModel
  autoResizeMode = Table.AutoResizeMode.Off

  val columnModel = peer.getColumnModel()
  columnModel.getColumn(0).setMaxWidth(50)
  columnModel.getColumn(1).setMaxWidth(100)

  selection.elementMode = Table.ElementMode.Row
  selection.intervalMode = Table.IntervalMode.Single
  selection.reactions += {
    case x: TableRowsSelected =>
      if (x.adjusting) {
        val diagnostics = model.asInstanceOf[DetectiveTableModel].diagnostics
        for (row <- selection.rows) {
          onClick(diagnostics(row))
        }
      }
  }

  def set(newDiagnostics: Seq[Diagnostic]) {
    model.asInstanceOf[DetectiveTableModel].set(newDiagnostics)
  }
}

class DetectiveTableModel(var diagnostics: Seq[Diagnostic] = Seq.empty) extends AbstractTableModel {

  def set(newDiagnostics: Seq[Diagnostic]) {
    diagnostics = newDiagnostics
    fireTableDataChanged()
  }

  def getRowCount: Int = diagnostics.length

  def getColumnCount: Int = 3

  def getValueAt(line: Int, column: Int): Object = {
    val diagnostic = diagnostics(line)
    column match {
      case 0 => diagnostic.pos.line.toString
      case 1 => if (diagnostic.severity == 1) "BUG" else "warning"
      case 2 => diagnostic.msg
    }
  }

  override def getColumnName(column: Int) = columnNames(column)

  val columnNames = Array[String]("line", "category", "hint")
}
