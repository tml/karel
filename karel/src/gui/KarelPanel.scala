package gui

import java.io.File
import java.util.concurrent.atomic.AtomicReference

import scala.swing.Dimension
import scala.swing.Graphics2D
import scala.swing.Panel
import scala.swing.event.MousePressed

import javax.imageio.ImageIO
import logic.Karel
import logic.World

class KarelPanel(atomicKarel: AtomicReference[Karel]) extends Panel {

  val tileSize = 40

  val tiles = "0123456789abcdefghijk" map (tile => ImageIO.read(new File("tiles/" + tile + ".png")))

  val fredsDimension = new Dimension(400, 400)
  minimumSize = fredsDimension
  preferredSize = fredsDimension
  maximumSize = fredsDimension

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g);

    def drawTile(x: Int, y: Int, tile: Int) {
      g.drawImage(tiles(tile), x * tileSize, y * tileSize, null)
    }

    def drawNumber(x: Int, y: Int, n: Int) {
      val str = n.toString
      val width = str.length() * 9
      Font.paintStringAt(g)(x * tileSize + 19 - width / 2, y * tileSize + 12, str)
    }

    val karel = atomicKarel.get
    for (y <- 0 until World.HEIGHT) {
      for (x <- 0 until World.WIDTH) {
        drawTile(x, y, karel.world.wallsAt(x, y))
      }
    }

    for (bs <- karel.world.beepers) {
      val n = bs._2
      if (n == 1) {
        val pos = bs._1
        val x = pos._1
        val y = pos._2
        drawTile(x, y, 20)
      }
    }

    drawTile(karel.x, karel.y, 16 + karel.direction)

    for (bs <- karel.world.beepers) {
      val n = bs._2
      if (n > 1) {
        val pos = bs._1
        val x = pos._1
        val y = pos._2
        drawTile(x, y, 20)
        drawNumber(x, y, n)
      }
    }
  }

  listenTo(mouse.clicks)
  reactions += {
    case clicked: MousePressed =>
      val point = clicked.point
      val x = point.x / tileSize
      val y = point.y / tileSize
      atomicKarel.set(atomicKarel.get.toggleBeeper(x, y))
      repaint()
  }
}
