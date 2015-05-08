package logic

import scala.collection.immutable.VectorBuilder
import scala.util.Random

object Labyrinth {
  final val LABYRINTH =
    "0 0 0 0 0 0 0 0 0 0 0 0" +
      "/ / / / / / / / / / / /" +
      "0/2/3/3/3/3/3/3/3/3/2/0" +
      "/ / / / / / / / / / / /" +
      "0/3/4/4/4/4/4/4/4/4/3/0" +
      "/ / / / / / / / / / / /" +
      "0/3/4/4/4/4/4/4/4/4/3/0" +
      "/ / / / / / / / / / / /" +
      "0/3/4/4/4/4/4/4/4/4/3/0" +
      "/ / / / / / / / / / / /" +
      "0/3/4/4/4/4/4/4/4/4/3/0" +
      "/ / / / / / / / / / / /" +
      "0/3/4/4/4/4/4/4/4/4/3/0" +
      "/ / / / / / / / / / / /" +
      "0/3/4/4/4/4/4/4/4/4/3/0" +
      "/ / / / / / / / / / / /" +
      "0/3/4/4/4/4/4/4/4/4/3/0" +
      "/ / / / / / / / / / / /" +
      "0/3/4/4/4/4/4/4/4/4/3/0" +
      "/ / / / / / / / / / / /" +
      "0/2/3/3/3/3/3/3/3/3/2/0" +
      "/ / / / / / / / / / / /" +
      "0 0 0 0 0 0 0 0 0 0 0 0"

  final val WALL_X = 1
  final val WALL_Y = 23
  final val BEEPER_X = 2 * WALL_X
  final val BEEPER_Y = 2 * WALL_Y

  final val START_POSITION = BEEPER_Y + BEEPER_X

  final val WALLS = Array(WALL_X, -WALL_Y, -WALL_X, WALL_Y, WALL_X, -WALL_Y, -WALL_X)
  final val BEEPERS = Array(BEEPER_X, -BEEPER_Y, -BEEPER_X, BEEPER_Y, BEEPER_X, -BEEPER_Y, -BEEPER_X)
}

class Labyrinth {
  import Labyrinth._

  private val lab = LABYRINTH.map(_.toInt).toArray
  private val rng = new Random()

  private case class Solution(lab: Array[Int], pos: Int) extends Exception

  val result: World = {
    try {
      destinationOpen(START_POSITION, 0, 100)
      null
    } catch {
      case Solution(lab, target) =>
        val vb = new VectorBuilder[Long]

        for (y <- 1 to 10) {
          var line = 0L
          for (x <- 1 to 10) {
            val pos = y * BEEPER_Y + x * BEEPER_X
            val a = lab(pos + WALL_X) & 1
            val b = lab(pos - WALL_Y) & 2
            val c = lab(pos - WALL_X) & 4
            val d = lab(pos + WALL_Y) & 8
            line = (line << 4) | a | b | c | d
          }
          vb += (line << 24)
        }
        val x = target % BEEPER_Y / BEEPER_X - 1
        val y = target / BEEPER_Y - 1
        World(vb.result).dropBeeper(x, y)

      case tooSlow: InterruptedException =>
        null
    }
  }

  private def causesPartition(pos: Int, dir: Int): Boolean = {
    (lab(pos + BEEPERS(dir)) <= '0') &&
      (lab(pos + BEEPERS(dir + 1)) > '0') &&
      (lab(pos + BEEPERS(dir + 3)) > '0')
  }

  private def destinationOpen(pos: Int, dir: Int, n: Int) {
    if (Thread.interrupted) throw new InterruptedException
    if (causesPartition(pos, dir)) return

    val beepers = lab(pos)
    lab(pos) = '0'
    if (n <= 1) throw Solution(lab, pos)

    val a = pos + BEEPER_X
    val b = pos - BEEPER_Y
    val c = pos - BEEPER_X
    val d = pos + BEEPER_Y

    lab(a) -= 1
    lab(b) -= 1
    lab(c) -= 1
    lab(d) -= 1

    var ones = 0
    if (lab(a) == '1') ones += 1
    if (lab(b) == '1') ones += 1
    if (lab(c) == '1') ones += 1
    if (lab(d) == '1') ones += 1

    var i = rng.nextInt(4)
    val j = i + 4

    if (ones == 0) {
      while (i < j) {
        val beeper = pos + BEEPERS(i)
        if (lab(beeper) >= '0') {
          val wall = pos + WALLS(i)
          lab(wall) = ' '
          destinationOpen(beeper, i & 3, n - 1)
          lab(wall) = '/'
        }
        i += 1
      }
    } else if (ones == 2) {
      while (i < j) {
        val beeper = pos + BEEPERS(i)
        if (lab(beeper) == '1') {
          val wall = pos + WALLS(i)
          lab(wall) = ' '
          destinationFound(beeper, i & 3, n - 1)
          lab(wall) = '/'
        }
        i += 1
      }
    } else if (ones == 1) {
      while (i < j) {
        val beeper = pos + BEEPERS(i)
        if (lab(beeper) >= '0') {
          val wall = pos + WALLS(i)
          lab(wall) = ' '
          if (lab(beeper) == '1') {
            destinationOpen(beeper, i & 3, n - 1)
          } else {
            destinationFound(beeper, i & 3, n - 1)
          }
          lab(wall) = '/'
        }
        i += 1
      }
    }

    lab(a) += 1
    lab(b) += 1
    lab(c) += 1
    lab(d) += 1

    lab(pos) = beepers
  }

  private def destinationFound(pos: Int, dir: Int, n: Int) {
    if (Thread.interrupted) throw new InterruptedException
    if (causesPartition(pos, dir)) return

    val beepers = lab(pos)
    lab(pos) = '0'
    if (n <= 1) throw Solution(lab, pos)

    val a = pos + BEEPER_X
    val b = pos - BEEPER_Y
    val c = pos - BEEPER_X
    val d = pos + BEEPER_Y

    lab(a) -= 1
    lab(b) -= 1
    lab(c) -= 1
    lab(d) -= 1

    var ones = 0
    if (lab(a) == '1') ones += 1
    if (lab(b) == '1') ones += 1
    if (lab(c) == '1') ones += 1
    if (lab(d) == '1') ones += 1

    var i = rng.nextInt(4)
    val j = i + 4

    if (ones == 0) {
      while (i < j) {
        val beeper = pos + BEEPERS(i)
        if (lab(beeper) >= '0') {
          val wall = pos + WALLS(i)
          lab(wall) = ' '
          destinationFound(beeper, i & 3, n - 1)
          lab(wall) = '/'
        }
        i += 1
      }
    } else if (ones == 1) {
      while (i < j) {
        val beeper = pos + BEEPERS(i)
        if (lab(beeper) == '1') {
          val wall = pos + WALLS(i)
          lab(wall) = ' '
          destinationFound(beeper, i & 3, n - 1)
          lab(wall) = '/'
        }
        i += 1
      }
    }

    lab(a) += 1
    lab(b) += 1
    lab(c) += 1
    lab(d) += 1

    lab(pos) = beepers
  }
}
