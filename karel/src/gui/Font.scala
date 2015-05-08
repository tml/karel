package gui

import java.awt.Graphics2D
import java.io.File

import javax.imageio.ImageIO

object Font {

  private val fontImage = ImageIO.read(new File("font.png"))
  val fontHeight = fontImage.getHeight()
  val fontWidth = fontImage.getWidth() / 96

  def paintCharacter(g: Graphics2D)(x: Int, y: Int, c: Char) {
    paintCharacterAt(g)(x * fontWidth, y * fontHeight, c)
  }

  def paintString(g: Graphics2D)(x0: Int, y: Int, s: Traversable[Char]) {
    paintStringAt(g)(x0 * fontWidth, y * fontHeight, s)
  }

  def paintCharacterAt(g: Graphics2D)(x: Int, y: Int, c: Char) {
    val index = c - 32
    if ((index >= 0) && (index < 96)) {
      val sx = index * fontWidth
      g.drawImage(fontImage, x, y, x + fontWidth, y + fontHeight, sx, 0, sx + fontWidth, fontHeight, null)
    }
  }

  def paintStringAt(g: Graphics2D)(x0: Int, y: Int, s: Traversable[Char]) {
    var x = x0
    for (c <- s) {
      paintCharacterAt(g)(x, y, c)
      x += fontWidth
    }
  }
}
