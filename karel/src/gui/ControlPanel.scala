package gui

import scala.swing._
import scala.swing.event._

class ControlPanel(problems: Seq[String]) extends BoxPanel(Orientation.Vertical) {

  val goal = new Button("goal")

  var problemPicker = new ComboBox(problems) {
    maximumSize = minimumSize
  }

  val start_stop_reset = new Button("start")

  val slider = new Slider {
    max = 11
    value = 0

    def delay = if (value == 0) -1 else (max - value)
  }

  val firstRow = new BoxPanel(Orientation.Horizontal) {
    contents += goal
    contents += problemPicker
    contents += start_stop_reset
  }

  val stepInto = new Button("step into (F12)")
  val stepOver = new Button("step over")
  val stepReturn = new Button("step return")

  def disableStepButtons() {
    stepInto.enabled = false
    stepOver.enabled = false
    stepReturn.enabled = false
  }

  disableStepButtons()

  def enableStepButtons() {
    stepInto.enabled = true
    stepOver.enabled = true
    stepReturn.enabled = true
  }

  val secondRow = new BoxPanel(Orientation.Horizontal) {
    contents += stepInto
    contents += stepOver
    contents += stepReturn
    border = Swing.EmptyBorder(16)
  }

  contents += firstRow
  contents += secondRow
  contents += slider

  def executionStarted() {
    problemPicker.enabled = false
    goal.enabled = false
    start_stop_reset.text = "stop"
    enableStepButtons()
  }

  def executionFinished() {
    problemPicker.enabled = true
    goal.enabled = true
    start_stop_reset.text = "reset"
    disableStepButtons()
  }
}
