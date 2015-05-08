package gui

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Desktop
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.Shape
import java.awt.event.ActionEvent
import java.awt.event.InputEvent
import java.awt.event.MouseWheelEvent
import java.awt.event.MouseWheelListener
import java.awt.geom.Line2D
import java.io.File
import java.util.Calendar

import scala.swing.Dialog
import scala.swing.Swing

import org.fife.ui.rsyntaxtextarea.FileLocation
import org.fife.ui.rsyntaxtextarea.SyntaxConstants
import org.fife.ui.rsyntaxtextarea.TextEditorPane

import javax.swing.AbstractAction
import javax.swing.KeyStroke
import parsing.Diagnostic

class Editor extends TextEditorPane {

  setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_C)
  setAntiAliasingEnabled(true)
  setTabSize(4)
  setTabsEmulated(true)
  setDirty(false)
  setAnimateBracketMatching(false)
  configureHotKeys()
  configureMouse()
  setFont(new java.awt.Font("Monospaced", java.awt.Font.PLAIN, 16))

  private def configureMouse() {
    addMouseWheelListener(new MouseWheelListener() {
      override def mouseWheelMoved(e: MouseWheelEvent) {
        if ((e.getModifiersEx() & InputEvent.CTRL_DOWN_MASK) != 0) {
          changeFontSize(-e.getWheelRotation())
        } else {
          e.getComponent.getParent.dispatchEvent(e)
        }
      }
    })
  }

  val directory = System.getProperty("user.home") + "/karel"
  if (new File(directory).mkdir()) {
    println("created directory " + directory)
  }
  val filenamePrefix = directory + "/karel"
  val filenameSuffix = ".txt"
  val filename = filenamePrefix + filenameSuffix

  private def backupFilename: String = {
    import java.util.Calendar

    val calendar = Calendar.getInstance
    val year = calendar.get(Calendar.YEAR) % 100
    val month = calendar.get(Calendar.MONTH) + 1
    val day = calendar.get(Calendar.DAY_OF_MONTH)

    val hour = calendar.get(Calendar.HOUR_OF_DAY)
    val minute = calendar.get(Calendar.MINUTE)
    val second = calendar.get(Calendar.SECOND)

    val time = "_%02d%02d%02d_%02d%02d%02d".format(year, month, day, hour, minute, second)
    filenamePrefix + time + filenameSuffix
  }

  load(FileLocation.create(filename), null)
  if (getText.isEmpty) {
    setText(firstProgram)
  }
  // see http://fifesoft.com/forum/viewtopic.php?f=10&t=695
  discardAllEdits()

  def goto(line: Int, column: Int) {
    gotoZeroBased(line - 1, column - 1)
  }

  private def gotoZeroBased(line: Int, column: Int) {
    lineOffset(line) match {
      case Some(offset) => setCaretPosition(offset + column)
      case None =>
    }
  }

  private def lineOffset(line: Int): Option[Int] = {
    // see http://www.coderanch.com/t/572006/GUI/java/JTextarea-line
    val elementOrNull = getDocument().getDefaultRootElement().getElement(line & Integer.MAX_VALUE)
    Option(elementOrNull).map(_.getStartOffset)
  }

  def goto(command: String) {
    val index = getText.indexOf("void " + command + "(")
    if (index != -1) {
      setCaretPosition(getLastVisibleOffset)
      Swing.onEDT {
        setCaretPosition(index)
      }
    }
  }

  def tryToSaveCode() {
    if (isDirty) {
      tryToSaveCodeAs(filename)
      tryToSaveCodeAs(backupFilename)
    }
  }

  private def tryToSaveCodeAs(as: String) {
    try {
      println("saving code as " + as)
      saveAs(FileLocation.create(as))
    } catch {
      case ex: Throwable => println(ex)
    }
  }

  def tryToOpenDirectory() {
    try {
      if (Desktop.isDesktopSupported()) {
        val desktop = Desktop.getDesktop
        if (desktop.isSupported(Desktop.Action.OPEN)) {
          desktop.open(new File(directory))
        }
      }
    } catch {
      case ex: Throwable => println(ex)
    }
  }

  private def configureHotKeys() {
    configureHotkey("F1", replaceSelection("moveForward();"))
    configureHotkey("F2", replaceSelection("turnLeft();"))
    configureHotkey("F3", replaceSelection("turnAround();"))
    configureHotkey("F4", replaceSelection("turnRight();"))

    configureHotkey("F5", replaceSelection("pickBeeper();"))
    configureHotkey("F6", replaceSelection("dropBeeper();"))
    configureHotkey("F7", replaceSelection("onBeeper()"))
    configureHotkey("F8", replaceSelection("beeperAhead()"))

    configureHotkey("F9", replaceSelection("leftIsClear()"))
    configureHotkey("F10", replaceSelection("frontIsClear()"))
    configureHotkey("F11", replaceSelection("rightIsClear()"))

    configureHotkey("ctrl SPACE", autoComplete)
    configureHotkey("ctrl shift I", autoIndent)
    configureHotkey("ctrl shift R", {
      callStackVisible = !callStackVisible
      repaint()
    })
  }

  // A nice wrapper around Javas's absolutely awful Key Binding API
  def configureHotkey(hotkey: String, callback: => Unit) {
    val action = new AbstractAction {
      override def actionPerformed(ignored: ActionEvent) {
        callback
      }
    }
    getInputMap.put(KeyStroke.getKeyStroke(hotkey), action)
    // Yes, we abuse the (unique) action object as an intermediate key :)
    getActionMap.put(action, action)
  }

  private def autoComplete() {
    moveToEndOfCurrentIdentifier()
    new AutoCompleter(getText, lineUntilCursor).result match {
      case Left(suffixes) => println(suffixes.mkString(", "))
      case Right(lcp) => insert(lcp, getCaretPosition)
    }
  }

  private def moveToEndOfCurrentIdentifier() {
    while (Character.isJavaIdentifierPart(getText(getCaretPosition(), 1).charAt(0))) {
      setCaretPosition(getCaretPosition() + 1)
    }
  }

  private def lineUntilCursor: String = {
    val offset = getLineStartOffsetOfCurrentLine
    getText(offset, getCaretPosition - offset)
  }

  def showDiagnostic(diagnostic: Diagnostic, title: String) {
    goto(diagnostic.pos.line, diagnostic.pos.column)
    requestFocus()
    Dialog.showMessage(null, diagnostic.msg, title, Dialog.Message.Error)
  }

  private def autoIndent() {
    val line = getCaretLineNumber
    setText(AutoIndenter.indent(getText))
    gotoEndOfLine(line)
  }

  private def gotoEndOfLine(line: Int) {
    lineOffset(line + 1).foreach(offset => setCaretPosition(offset - 1))
  }

  // Warning: Do NOT refactor firstProgram into a readable multi-line string literal!
  // This may result in double line breaks when the editor loads the code again.
  // see http://stackoverflow.com/questions/17190242/
  private def firstProgram = "void karelsFirstProgram()\n{\n    moveForward();\n    pickBeeper();\n    moveForward();\n    turnLeft();\n    moveForward();\n    myTurnRight();\n    moveForward();\n    dropBeeper();\n    moveForward();\n}\n\nvoid myTurnRight()\n{\n    turnLeft();\n    turnLeft();\n    turnLeft();\n}\n"

  private var calls = List.empty[(Int, Int)]
  private var lines = List.empty[Shape]

  private var fm = getFontMetrics(getFont)
  private var fontHeight = fm.getHeight()
  private var fontWidth = fm.charWidth('@')
  private var thickness = fontWidth - 2

  private var stroke = new BasicStroke(thickness, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
  private val color = new Color(1.0f, 0, 0, 0.25f)

  private def updateFontMetrics() {
    fm = getFontMetrics(getFont)
    fontHeight = fm.getHeight()
    fontWidth = fm.charWidth('@')
    thickness = fontWidth - 2
    stroke = new BasicStroke(thickness, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
  }

  def push(callerLine: Int, calleeLine: Int) {
    if (callerLine > 0) {
      calls = (callerLine, calleeLine) :: calls
      pushLine(callerLine, calleeLine)
      repaint()
    }
  }

  private def pushLine(callerLine: Int, calleeLine: Int) {
    val x = 0.5 * thickness + lines.length * fontWidth
    val y1 = (callerLine - 0.5) * fontHeight
    val y2 = (calleeLine - 0.5) * fontHeight

    val line = new Line2D.Double(x, y1, x, y2)
    lines = line :: lines
  }

  private def changeFontSize(delta: Float) {
    val font = getFont
    val desiredSize = font.getSize() + delta
    if ((desiredSize >= 10) && (desiredSize <= 100)) {
      println("setting font size to " + desiredSize)
      setFont(font.deriveFont(desiredSize))
      updateLines()
    }
  }

  private def updateLines() {
    updateFontMetrics()
    lines = Nil
    for ((callerLine, calleeLine) <- calls.reverse) {
      pushLine(callerLine, calleeLine)
    }
    repaint()
  }

  def pop() {
    if (!calls.isEmpty) {
      calls = calls.tail
      lines = lines.tail
      repaint()
    }
  }

  def clearStack() {
    calls = Nil
    lines = Nil
    repaint()
  }

  override def paint(g: Graphics) {
    super.paint(g)
    if (callStackVisible) {
      paintCallStack(g.create().asInstanceOf[Graphics2D])
    }
  }

  private var callStackVisible = false

  private def paintCallStack(g2d: Graphics2D) {
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2d.setStroke(stroke)
    g2d.setColor(color)
    if (!lines.isEmpty) {
      lines.foreach(g2d.draw)
      g2d.draw(lines.head)
    }
  }
}
