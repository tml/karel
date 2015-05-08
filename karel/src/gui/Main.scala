package gui

import java.util.concurrent.atomic.AtomicReference

import scala.swing.BorderPanel
import scala.swing.BoxPanel
import scala.swing.ComboBox.stringEditor
import scala.swing.Component
import scala.swing.Dialog
import scala.swing.Dimension
import scala.swing.MainFrame
import scala.swing.Orientation
import scala.swing.SimpleSwingApplication
import scala.swing.Swing
import scala.swing.event.ButtonClicked
import scala.swing.event.SelectionChanged
import scala.swing.event.ValueChanged

import org.fife.ui.rtextarea.RTextScrollPane

import detective.Detective
import javax.swing.Timer
import logic.Goals
import logic.KarelError
import logic.World
import parsing.KarelParser
import parsing.KarelSemantics
import vm.CodeGenerator
import vm.Instruction
import vm.VM

object Main extends SimpleSwingApplication {

  def top = new MainFrame {

    var initialKarel = World.karelsFirstProgram
    val atomicKarel = new AtomicReference(initialKarel)

    def currentIndex = controlPanel.problemPicker.selection.index
    def currentProblem = World.problemMethods(currentIndex)
    def entryPoint = World.problemNames(currentIndex)
    def currentLevel = controlPanel.problemPicker.item.charAt(0) - '0'

    val controlPanel = new ControlPanel(World.problemDirectory)
    val karelPanel = new KarelPanel(atomicKarel)
    val detectiveTable = new DetectiveTable(diagnostic => {
      editor.goto(diagnostic.pos.line, diagnostic.pos.column)
      editor.requestFocusInWindow()
    })
    val editor = new Editor()
    val vmPanel = new VMPanel(editor.getFont)

    var showVirtualMachine = false
    var optimizeTailCalls = false

    editor.configureHotkey("ctrl shift M", {
      showVirtualMachine = !showVirtualMachine
      vmPanel.visible = showVirtualMachine
    })
    editor.configureHotkey("ctrl shift T", optimizeTailCalls = !optimizeTailCalls)
    editor.configureHotkey("F12",
      if (controlPanel.start_stop_reset.text == "stop") stepInto else controlPanel.start_stop_reset.doClick)
    editor.configureHotkey("ctrl shift D", processProgram { program =>
      val hints = new Detective(program).result
      detectiveTable.set(hints)
    })

    controlPanel.goal.reactions += {
      case ButtonClicked(_) =>
        atomicKarel.set(initialKarel)
        karelPanel.repaint()
        Goals.goals.get(entryPoint).foreach(executeGoal)
        controlPanel.stepOver.enabled = false
        controlPanel.stepReturn.enabled = false
        editor.requestFocusInWindow()
    }

    controlPanel.problemPicker.selection.reactions += {
      case SelectionChanged(_) =>

        controlPanel.start_stop_reset.text = "start"

        initialKarel = World(currentProblem)
        atomicKarel.set(initialKarel)
        karelPanel.repaint()

        editor.requestFocusInWindow()
        editor.goto(entryPoint)
    }

    def stop() {
      timer.stop()
      controlPanel.executionFinished()
      vmPanel.clearStack()
      editor.clearStack()
      editor.requestFocusInWindow()
    }

    def update() {
      val instruction = vm.currentInstruction
      val line = instruction.line
      if (line > 0) {
        editor.goto(line, instruction.column)
        vmPanel.update(vm.PC, vm.getStack)
      }
      karelPanel.repaint()
    }

    def step(how: => Unit) {
      try {
        how
        update()
      } catch {
        case ex: NoSuchElementException if ex.getMessage() == "head of empty list" =>
          stop()
          update()
        case KarelError(msg) =>
          stop()
          update()
          Dialog.showMessage(null, msg, "Runtime Error", Dialog.Message.Error)
      }
    }

    controlPanel.start_stop_reset.reactions += {
      case ButtonClicked(_) =>
        editor.requestFocusInWindow()
        controlPanel.start_stop_reset.text match {
          case "start" =>
            parseAndExecute()

          case "stop" =>
            stop()

          case "reset" =>
            atomicKarel.set(initialKarel)
            karelPanel.repaint()
            controlPanel.start_stop_reset.text = "start"
        }
    }

    def stepInto() {
      step(vm.stepInto(showVirtualMachine))
      editor.requestFocusInWindow()
    }

    controlPanel.stepInto.reactions += {
      case ButtonClicked(_) =>
        stepInto()
    }

    controlPanel.stepOver.reactions += {
      case ButtonClicked(_) =>
        step(vm.stepOver())
        editor.requestFocusInWindow()
    }

    controlPanel.stepReturn.reactions += {
      case ButtonClicked(_) =>
        step(vm.stepReturn())
        editor.requestFocusInWindow()
    }

    def isRunning = controlPanel.start_stop_reset.text == "stop"

    var timer = new Timer(1000, new java.awt.event.ActionListener {
      override def actionPerformed(event: java.awt.event.ActionEvent) {
        step(vm.stepInto(showVirtualMachine))
      }
    })

    def delay = {
      val wait = controlPanel.slider.delay
      if (wait < 0) wait else 1 << wait
    }

    controlPanel.slider.reactions += {
      case _: ValueChanged =>
        editor.requestFocusInWindow()
        val d = delay
        if (d < 0) {
          timer.stop()
        } else {
          timer.setInitialDelay(d)
          timer.setDelay(d)
          if (isRunning) {
            timer.restart()
          }
        }
    }

    def processProgram(how: KarelSemantics => Unit) {
      editor.tryToSaveCode()
      val input = editor.getText
      KarelParser.syntacticParse(input) match {
        case Left(diagnostic) =>
          editor.showDiagnostic(diagnostic, "Syntax Error")

        case Right(program) => {
          val semantics = new KarelSemantics(program, entryPoint, currentLevel)
          val errors = semantics.errors
          if (errors.isEmpty) {

            how(semantics)

          } else {
            editor.showDiagnostic(errors.head, "Semantic Error")
          }
        }
      }
    }

    def parseAndExecute() {
      processProgram { semantics =>
        if (semantics.defs.contains(entryPoint)) {
          val instructions = new CodeGenerator(semantics, optimizeTailCalls).result
          vmPanel.setProgram(instructions)
          start(instructions)
        } else {
          Dialog.showMessage(null, "undefined command " + entryPoint, "Missing entry point", Dialog.Message.Error)
        }
      }
    }

    var vm: VM = null

    def start(instructions: Seq[Instruction]) {
      vm = new VM(instructions, atomicKarel, editor.push(_, _), editor.pop(), infiniteLoopDetected())
      controlPanel.executionStarted()
      update()
      if (delay >= 0) {
        timer.start()
      }
    }

    def infiniteLoopDetected() {
      Dialog.showMessage(null, "Please check your program for infinite loops!", "Timeout expired", Dialog.Message.Error)
    }

    def executeGoal(goal: String) {
      val instructions = Instruction.dummyBuffer()
      instructions.appendAll(goal.map(Instruction(_)))
      if (util.Configuration.privileged) {
        vmPanel.setProgram(instructions)
      }
      start(instructions)
    }

    title = "Karel the Robot 2014"

    contents = new BorderPanel {

      val left = new BoxPanel(Orientation.Vertical) {
        contents += controlPanel
        contents += karelPanel
        contents += detectiveTable
        border = Swing.EmptyBorder(16)
      }

      val editorWithScrolling = new Component {
        override lazy val peer = new RTextScrollPane(editor)
        preferredSize = new Dimension(85 * 7, 1)
      }

      add(left, BorderPanel.Position.West)
      add(editorWithScrolling, BorderPanel.Position.Center)
      add(vmPanel, BorderPanel.Position.East)
    }

    override def closeOperation() {
      editor.tryToSaveCode()
      if (!util.Configuration.privileged) {
        editor.tryToOpenDirectory()
      }
      super.closeOperation()
    }

    editor.requestFocusInWindow()
    pack()
  }
}
