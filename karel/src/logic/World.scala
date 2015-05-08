package logic

import scala.collection.immutable.VectorBuilder

case class World(val walls: Vector[Long], val beepers: Map[(Int, Int), Int] = Map.empty) {

  import World._

  // Each cell takes up 4 bits (for the 4 walls), hence the shift by 2.
  // The world is internally flipped horizontically, hence the xor with 15.
  // (We are using hex literals to write down some simple worlds,
  // and those start with the most significant digit on the left.) 
  private def X(x: Int) = ((x ^ 15) << 2)

  // Calculates the appropriate bitmask to access and update cells.
  private def bitmask(x: Int, directions: Long): Long = directions << X(x)

  def isClear(x: Int, y: Int, direction: Int): Boolean = {
    (wallsAt(x, y) & (1L << direction)) == WALL_NONE
  }

  def validPosition(x: Int, y: Int): Boolean = {
    (x >= 0) && (x < WIDTH) && (y >= 0) && (y < HEIGHT)
  }

  def wallsAt(x: Int, y: Int): Int = {
    if (validPosition(x, y))
      (walls(y) >> X(x)).toInt & WALL_ALL
    else
      WALL_NONE
  }

  def beeperAt(x: Int, y: Int): Boolean = {
    validPosition(x, y) && beepers.contains((x, y))
  }

  def beepersAt(x: Int, y: Int): Int = {
    beepers.getOrElse((x, y), 0)
  }

  def pickBeeper(x: Int, y: Int): World = {
    val newBeepers = beepers.get((x, y)) match {
      case None => beepers // TODO error handling?
      case Some(1) => beepers - ((x, y))
      case Some(n) => beepers + (((x, y), n - 1))
    }
    copy(beepers = newBeepers)
  }

  def pickAllBeepers(x: Int, y: Int): World = {
    copy(beepers = beepers - ((x, y)))
  }

  def dropBeeper(x: Int, y: Int, n: Int = 1): World = {
    val newBeepers = beepers.get((x, y)) match {
      case None => beepers + (((x, y), n))
      case Some(m) => beepers + (((x, y), m + n))
    }
    copy(beepers = newBeepers)
  }

  def dropBeepers(positions: (Int, Int)*): World = {
    positions.foldLeft(this)((world: World, pos: (Int, Int)) => world.dropBeeper(pos._1, pos._2))
  }

  def toggleBeeper(x: Int, y: Int): World = {
    if (beeperAt(x, y)) pickBeeper(x, y) else dropBeeper(x, y)
  }

  def buildHorizontalWall(x: Int, y: Int): World = {
    var newWalls = walls
    if (y < HEIGHT) {
      newWalls = newWalls.updated(y, walls(y) | bitmask(x, WALL_NORTH))
    }
    if (y > 0) {
      newWalls = newWalls.updated(y - 1, walls(y - 1) | bitmask(x, WALL_SOUTH))
    }
    copy(walls = newWalls)
  }

  def tearDownHorizontalWall(x: Int, y: Int) = {
    var newWalls = walls
    if (y < HEIGHT) {
      newWalls = newWalls.updated(y, walls(y) & ~bitmask(x, WALL_NORTH))
    }
    if (y > 0) {
      newWalls = newWalls.updated(y - 1, walls(y - 1) & ~bitmask(x, WALL_SOUTH))
    }
    copy(walls = newWalls)
  }

  def buildVerticalWall(x: Int, y: Int): World = {
    var newWalls = walls(y)
    if (x < WIDTH) {
      newWalls |= bitmask(x, WALL_WEST)
    }
    if (x > 0) {
      newWalls |= bitmask(x - 1, WALL_EAST)
    }
    copy(walls = walls.updated(y, newWalls))
  }

  def tearDownVerticalWall(x: Int, y: Int): World = {
    var newWalls = walls(y)
    if (x < WIDTH) {
      newWalls &= ~bitmask(x, WALL_WEST)
    }
    if (x > 0) {
      newWalls &= ~bitmask(x - 1, WALL_EAST)
    }
    copy(walls = walls.updated(y, newWalls))
  }

  def tearDownWall(x: Int, y: Int, direction: Int): World = {
    tearDownWalls(x, y, 1L << direction)
  }

  def tearDownWalls(x: Int, y: Int, directions: Long): World = {
    val newWalls = walls(y) & ~bitmask(x, directions)
    copy(walls = walls.updated(y, newWalls))
  }
}

object World {

  final val HEIGHT = 10
  final val WIDTH = 10

  final val EAST = 0
  final val NORTH = 1
  final val WEST = 2
  final val SOUTH = 3

  final val WALL_NONE = 0
  final val WALL_EAST = 1L
  final val WALL_NORTH = 2L
  final val WALL_WEST = 4L
  final val WALL_SOUTH = 8L
  final val WALL_ALL = 15

  val emptyWorld = World(Vector(
    0x6222222223000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0xc888888889000000L))

  val street = World(Vector(
    0x6222222223000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0xc000000001000000L,
    0x3c08080809000000L,
    0x03d7d7d7d6000000L))

  val helloWorld = World(Vector(
    0x0000000000000000L,
    0x0000000000000000L,
    0x0000000000000000L,
    0x0000000000000000L,
    0x0000000000000000L,
    0x0000000000000000L,
    0x0000000000000000L,
    0x6222300000000000L,
    0x4008900000000000L,
    0xc890000000000000L))

  val stairs = World(Vector(
    0x6222222223000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000801000000L,
    0x4000009741000000L,
    0x4000096141000000L,
    0x4000960141000000L,
    0x4009600141000000L,
    0x4096000141000000L,
    0xc9e88889c9000000L))

  val mountain = World(Vector(
    0x6222222223000000L,
    0x4000080001000000L,
    0x4000174001000000L,
    0x400095c001000000L,
    0x4001603401000000L,
    0x4009401c01000000L,
    0x4016000341000000L,
    0x40940001c1000000L,
    0x4160000035000000L,
    0xc9c888889d000000L))

  val hundredBeepers = for (y <- 0 to HEIGHT; x <- 0 to WIDTH) yield (x, y)

  val mazeWorld = World(Vector(
    0xffffffffff000000L,
    0xffffffffff000000L,
    0xffffffffff000000L,
    0xffffffffff000000L,
    0xffffffffff000000L,
    0xffffffffff000000L,
    0xffffffffff000000L,
    0xffffffffff000000L,
    0xffffffffff000000L,
    0xffffffffff000000L))
    .dropBeepers(hundredBeepers: _*)

  val trapWorld = World(Vector(
    0x6222222223000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0x4000000001000000L,
    0xeaaaaaaaab000000L))

  val pascalsWorld = World(Vector(
    0x8080808080000000L,
    0xf5f5e161f5000000L,
    0x6174b5c175000000L,
    0x0000202000000000L,
    0x6222222230000000L,
    0x4000000010000000L,
    0x4000000010000000L,
    0x4000000010000000L,
    0x4000000010000000L,
    0xc888888890000000L))

  def pillars: World = {
    var world = emptyWorld

    for (x <- 0 to 9) {
      for (y <- 0 until rng.nextInt(11)) {
        world = world.dropBeeper(x, 9 - y)
      }
    }
    world
  }

  val rng = new scala.util.Random()

  val problemSets = Vector(
    Vector(
      Vector("karelsFirstProgram")),

    Vector(
      Vector("obtainArtefact", "defuseOneBomb", "defuseTwoBombs", "practiceHomerun"),
      Vector("climbTheStairs", "fillTheHoles", "saveTheFlower", "mowTheLawn"),
      Vector("harvestTheField", "repairTheStreet", "cleanTheRoom", "tileTheFloor"),
      Vector("stealOlympicFire", "removeTheTiles", "walkTheLabyrinth")),

    Vector(
      Vector("hangTheLampions", "followTheSeeds", "cleanTheTunnels"),
      Vector("increment", "decrement", "addSlow"),
      Vector("saveTheFlowers", "findTeddyBear", "jumpTheHurdles"),
      Vector("solveTheMaze", "quantize", "addFast")),

    Vector(
      Vector("partyAgain", "fetchTheStars"),
      Vector("secureTheCave", "layAndRemoveTiles"),
      Vector("countTheLines", "countTheBeepers"),
      Vector("pascalsTriangle", "findShelters")))

  val problemDirectory = {
    val problems = new VectorBuilder[String]
    var a = 0
    for (sets <- World.problemSets) {
      var b = 1
      for (set <- sets) {
        var c = 1
        for (problem <- set) {
          problems += "%d.%d.%d %s".format(a, b, c, problem)
          c += 1
        }
        b += 1
      }
      a += 1
    }
    problems.result
  }

  val problemNames = problemSets.flatten.flatten

  val problemMethods = problemNames.map(getClass.getMethod(_))

  def apply(problemMethod: java.lang.reflect.Method) = problemMethod.invoke(this).asInstanceOf[Karel]

  val karelsFirstProgram: Karel = {
    var world = helloWorld.dropBeeper(1, 9)

    Karel(0, 9, EAST, world)
  }

  def obtainArtefact: Karel = {
    var world = emptyWorld.buildVerticalWall(5, 5).dropBeeper(6, 5)

    Karel(3, 5, EAST, world)
  }

  def defuseOneBomb: Karel = {
    var world = emptyWorld.dropBeeper(9, 9)
    Karel(0, 9, EAST, world)
  }

  def defuseTwoBombs: Karel = {
    var world = emptyWorld.dropBeepers((0, 0), (9, 9))
    Karel(0, 9, EAST, world)
  }

  def practiceHomerun: Karel = {
    var world = emptyWorld.dropBeepers((0, 0), (9, 0), (0, 9), (9, 9))
    Karel(0, 9, EAST, world)
  }

  def fillTheHoles: Karel = {
    Karel(1, 8, EAST, street)
  }

  def climbTheStairs: Karel = {
    Karel(0, 9, EAST, stairs)
  }

  def saveTheFlower: Karel = {
    Karel(0, 9, EAST, mountain.dropBeeper(1, 9))
  }

  def mowTheLawn: Karel = {
    var world = emptyWorld
    for (y <- 2 until 8) {
      for (x <- 2 until 8) {
        world = world.dropBeeper(x, y)
      }
    }
    Karel(1, 7, EAST, world)
  }

  def harvestTheField: Karel = {
    var world = emptyWorld
    for (i <- 0 until 4) {
      for (k <- 0 until 4) {
        world = world.dropBeeper(5 - i + k, 1 + i + k)
      }
    }
    Karel(5, 8, NORTH, world)
  }

  def repairTheStreet: Karel = {
    var world = emptyWorld

    for (i <- 0 to 9) {
      if (rng.nextBoolean()) {
        world = world.buildHorizontalWall(i, 9)
      } else {
        world = world.buildVerticalWall(i, 9)
        world = world.buildVerticalWall(i + 1, 9)
      }
    }

    Karel(0, 8, EAST, world)
  }

  def cleanTheRoom: Karel = {
    var world = emptyWorld

    for (y <- 0 to 9) {
      for (x <- 0 to 9) {
        if (rng.nextBoolean()) {
          world = world.dropBeeper(x, y)
        }
      }
    }
    Karel(0, 9, EAST, world)
  }

  def tileTheFloor: Karel = {
    Karel(0, 9, EAST, emptyWorld)
  }

  def stealOlympicFire: Karel = {
    Karel(0, 9, EAST, stairs.dropBeeper(7, 3))
  }

  def removeTheTiles: Karel = {
    Karel(0, 9, EAST, emptyWorld.dropBeepers(hundredBeepers: _*))
  }

  /*
   * Sometimes, due to "bad luck" with the random number generator,
   * the labyrinth generation algorithm causes a noticeable pause.
   * A practical fix is to run the function multiple times in parallel
   * and let the fastest function "win".
   */
  def walkTheLabyrinth: Karel = {
    import java.util.concurrent._

    class MyCallable extends Callable[Karel] {
      def call = Karel(0, 0, EAST, new Labyrinth().result).turnAwayFromWall
    }

    val tasks = 16
    val pool = Executors.newFixedThreadPool(tasks)
    val service = new ExecutorCompletionService[Karel](pool)

    for (_ <- 0 until tasks) {
      service.submit(new MyCallable)
    }

    val quickest = service.take().get()
    pool.shutdownNow()
    quickest
  }

  def hangTheLampions: Karel = {
    var world = emptyWorld.dropBeepers((for (x <- 0 to 9) yield (x, 9)): _*)
    for (x <- 0 to 9) {
      world = world.buildHorizontalWall(x, 1 + rng.nextInt(3))
    }
    Karel(0, 9, EAST, world)
  }

  def followTheSeeds: Karel = {
    var world = emptyWorld.dropBeepers(
      (4, 4), (4, 5),
      (5, 5), (6, 5),
      (6, 4), (6, 3), (6, 2),
      (5, 2), (4, 2), (3, 2), (2, 2),
      (2, 3), (2, 4), (2, 5), (2, 6), (2, 7),
      (3, 7), (4, 7), (5, 7), (6, 7), (7, 7), (8, 7),
      (8, 6), (8, 5), (8, 4), (8, 3), (8, 2), (8, 1), (8, 0),
      (7, 0), (6, 0), (5, 0), (4, 0), (3, 0), (2, 0), (1, 0), (0, 0),
      (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8), (0, 9),
      (1, 9), (2, 9), (3, 9), (4, 9), (5, 9), (6, 9), (7, 9), (8, 9), (9, 9))

    Karel(5, 4, WEST, world)
  }

  def cleanTheTunnels = Karel(0, 9, EAST, pillars)

  def increment: Karel = {
    var world = emptyWorld

    for (x <- 2 to 9) {
      if (rng.nextBoolean()) {
        world = world.dropBeeper(x, 0)
      }
    }
    Karel(9, 0, WEST, world)
  }

  def decrement = increment

  def addSlow: Karel = {
    var world = emptyWorld

    for (y <- 0 to 1) {
      for (x <- 2 to 9) {
        if (rng.nextBoolean()) {
          world = world.dropBeeper(x, y)
        }
      }
    }
    Karel(9, 0, WEST, world)
  }

  def saveTheFlowers: Karel = {
    var world = emptyWorld

    var y1 = rng.nextInt(5)
    var y2 = rng.nextInt(1 + y1)
    var y3 = rng.nextInt(1 + y2)
    var y4 = rng.nextInt(1 + y3)
    y1 += 5
    y2 += 4
    y3 += 3
    y4 += 2
    val b = Array(rng.nextInt(8) + 2, rng.nextInt(8) + 2, rng.nextInt(8) + 2).sorted
    for (y <- y1 until 10) world = world.buildVerticalWall(1, y)
    world = world.buildHorizontalWall(1, y1)
    for (y <- y2 until y1) world = world.buildVerticalWall(2, y)
    world = world.buildHorizontalWall(2, y2)
    for (y <- y3 until y2) world = world.buildVerticalWall(3, y)
    world = world.buildHorizontalWall(3, y3)
    for (y <- y4 until y3) world = world.buildVerticalWall(4, y)
    world = world.buildHorizontalWall(4, y4)
    for (y <- 1 until y4) world = world.buildVerticalWall(5, y)

    world = world.dropBeepers((1, y1 - 1), (2, y2 - 1), (3, y3 - 1), (4, y4 - 1))
    world = world.buildHorizontalWall(5, 1)

    var y7 = rng.nextInt(6)
    var y6 = rng.nextInt(1 + y7)
    var y5 = rng.nextInt(1 + y6)
    y7 += 4
    y6 += 3
    y5 += 2
    for (y <- 1 until y5) world = world.buildVerticalWall(6, y)
    world = world.buildHorizontalWall(6, y5)
    for (y <- y5 until y6) world = world.buildVerticalWall(7, y)
    world = world.buildHorizontalWall(7, y6)
    for (y <- y6 until y7) world = world.buildVerticalWall(8, y)
    world = world.buildHorizontalWall(8, y7)
    for (y <- y7 until 10) world = world.buildVerticalWall(9, y)

    Karel(0, 9, EAST, world)
  }

  def findTeddyBear: Karel = {
    var world = emptyWorld
    val xy = rng.nextInt(10)
    rng.nextInt(4) match {
      case EAST =>
        world = world.dropBeeper(9, xy)
      case WEST =>
        world = world.dropBeeper(0, xy)
      case NORTH =>
        world = world.dropBeeper(xy, 0)
      case SOUTH =>
        world = world.dropBeeper(xy, 9)
    }
    val x = rng.nextInt(10)
    val y = rng.nextInt(10)
    val dir = rng.nextInt(4)
    Karel(x, y, dir, world)
  }

  def jumpTheHurdles: Karel = {
    val xBeeper = 5 + rng.nextInt(5)
    var world = emptyWorld.dropBeeper(xBeeper, 9)

    for (x <- 1 to xBeeper) {
      for (y <- 0 until rng.nextInt(10)) {
        world = world.buildVerticalWall(x, 9 - y)
      }
    }

    Karel(0, 9, EAST, world)
  }

  def solveTheMaze: Karel = {
    var karel = Karel(0, 0, 0, mazeWorld)

    def generateMaze() {
      val n = rng.nextInt(4)
      karel = karel.pickBeeper.turn(n)
      for (i <- 0 until 4) {
        if (karel.beeperAhead) {
          karel = karel.tearDownWall.moveForward
          generateMaze
          karel = karel.turnAround.tearDownWall.moveForward.turnAround
        }
        karel = karel.turnLeft
      }
      karel = karel.turn(-n)
    }
    generateMaze()
    val x = rng.nextInt(10)
    val y = rng.nextInt(10)
    Karel(0, 0, EAST, karel.world.dropBeeper(x, y))
  }

  def quantize: Karel = {
    Karel(0, 9, EAST, pillars)
  }

  def addFast: Karel = {
    var world = emptyWorld

    for (y <- 0 to 1) {
      for (x <- 2 to 9) {
        if (rng.nextBoolean()) {
          world = world.dropBeeper(x, y)
        }
      }
    }
    Karel(9, 0, SOUTH, world)
  }

  def partyAgain: Karel = {
    var world = trapWorld.dropBeepers((for (x <- 0 to 9) yield (x, 8)): _*)
    for (x <- 0 to 9) {
      world = world.buildHorizontalWall(x, 1 + rng.nextInt(3))
    }
    Karel(0, 8, EAST, world)
  }

  def fetchTheStars: Karel = {
    var world = trapWorld
    for (x <- 0 to 9) {
      val y = 1 + rng.nextInt(3)
      world = world.buildHorizontalWall(x, y).dropBeeper(x, y)
    }
    Karel(0, 8, EAST, world)
  }

  def secureTheCave: Karel = {
    var world = emptyWorld
    for (x <- 0 to 9) {
      val y = 1 + rng.nextInt(3)
      world = world.buildHorizontalWall(x, y).dropBeepers((for (a <- y to y + rng.nextInt(3)) yield (x, a)): _*)
    }
    Karel(0, 9, EAST, world)
  }

  def layAndRemoveTiles: Karel = {
    Karel(0, 9, EAST, emptyWorld)
  }

  def countTheLines = Karel(0, 0, EAST, cleanTheRoom.world, Int.MaxValue)

  def countTheBeepers = countTheLines

  def pascalsTriangle: Karel = {
    Karel(4, 9, NORTH, pascalsWorld, Int.MaxValue)
  }

  def findShelters: Karel = {
    var world = emptyWorld
    for (i <- 0 until 25) {
      world = world.buildHorizontalWall(rng.nextInt(10), 1 + rng.nextInt(9))
    }
    for (i <- 0 until 25) {
      world = world.buildVerticalWall(1 + rng.nextInt(9), rng.nextInt(10))
    }
    Karel(rng.nextInt(10), rng.nextInt(10), 0 * rng.nextInt(4), world, Int.MaxValue)
  }
}
