package logic

object Karel {
  val deltaX = Array(1, 0, -1, 0)
  val deltaY = Array(0, -1, 0, 1)
}

case class Karel(val x: Int, val y: Int, val direction: Int, val world: World, val maxBeepers: Int = 1) {

  def left = (direction + 1) & 3

  def right = (direction + 3) & 3

  def onBeeper: Boolean = world.beeperAt(x, y)

  def frontIsClear: Boolean = world.isClear(x, y, direction)

  def leftIsClear: Boolean = world.isClear(x, y, left)

  def rightIsClear: Boolean = world.isClear(x, y, right)

  def beeperAhead: Boolean = world.beeperAt(x + Karel.deltaX(direction), y + Karel.deltaY(direction))

  def moveForward: Karel = {
    if (!frontIsClear) throw new KarelError("cannot move through wall")
    copy(x = x + Karel.deltaX(direction), y = y + Karel.deltaY(direction))
  }

  def turn(delta: Int): Karel = copy(direction = (direction + delta) & 3)

  def turnLeft: Karel = turn(1)

  def turnRight: Karel = turn(3)

  def turnAround: Karel = turn(2)

  def pickBeeper: Karel = {
    if (!onBeeper) throw new KarelError("there is no beeper to pick")
    copy(world = world.pickBeeper(x, y))
  }

  def pickAllBeepers: Karel = {
    copy(world = world.pickAllBeepers(x, y))
  }

  def pickBeeperIn(directionDelta: Int): Karel = {
    val dir = (direction + directionDelta) & 3
    copy(world = world.pickBeeper(x + Karel.deltaX(dir), y + Karel.deltaY(dir)))
  }

  def dropBeeper: Karel = {
    if (world.beepersAt(x, y) == maxBeepers) throw new KarelError("cannot drop another beeper")
    copy(world = world.dropBeeper(x, y))
  }

  def toggleBeeper(x: Int, y: Int) = {
    copy(world = world.toggleBeeper(x, y))
  }

  def tearDownWall: Karel = copy(world = world.tearDownWall(x, y, direction))

  def turnAwayFromWall: Karel = {
    if (frontIsClear)
      this
    else if (leftIsClear)
      turnLeft
    else if (rightIsClear)
      turnRight
    else
      turnAround
  }
}
