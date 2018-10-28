package adrift.display

import adrift.Color
import adrift.display.glutil.{SpriteBatch, Texture}

import scala.collection.mutable

class GlyphRenderer(
  spriteBatch: SpriteBatch,
  tileWidth: Int,
  tileHeight: Int,
  screenTileWidth: Int,
  screenTileHeight: Int,
  font: Texture,
) {
  private val tilesPerRow: Int = font.width / tileWidth
  require(font.width / tileWidth.toFloat - tilesPerRow == 0)

  def drawChar(
    tex: Texture,
    x: Int,
    y: Int,
    c: Int,
    fg: Color = Color.White,
    bg: Color = Color.Black
  ): Unit = {
    if (bg.a != 0) {
      val cx = 0xdb % tilesPerRow
      val cy = 0xdb / tilesPerRow
      spriteBatch.drawRegion(
        tex,
        cx * tileWidth, cy * tileHeight,
        tileWidth, tileHeight,
        x * screenTileWidth, y * screenTileHeight,
        screenTileWidth, screenTileHeight,
        bg
      )
    }
    val cx = c % tilesPerRow
    val cy = c / tilesPerRow
    spriteBatch.drawRegion(
      tex,
      cx * tileWidth, cy * tileHeight,
      tileWidth, tileHeight,
      x * screenTileWidth, y * screenTileHeight,
      screenTileWidth, screenTileHeight,
      fg
    )
  }

  def drawBox(x: Int, y: Int, w: Int, h: Int): Unit = {
    import CP437.BoxDrawing
    drawChar(font, x, y, BoxDrawing.__RD)
    drawChar(font, x + w - 1, y, BoxDrawing.L__D)
    drawChar(font, x, y + h - 1, BoxDrawing._UR_)
    drawChar(font, x + w - 1, y + h - 1, BoxDrawing.LU__)
    for (iy <- 1 until (h - 1); ix <- 1 until (w - 1))
      drawChar(font, x + ix, y + iy, ' ')
    for (ix <- 1 until (w - 1)) {
      drawChar(font, x + ix, y, BoxDrawing.L_R_)
      drawChar(font, x + ix, y + h - 1, BoxDrawing.L_R_)
    }
    for (iy <- 1 until (h - 1)) {
      drawChar(font, x, y + iy, BoxDrawing._U_D)
      drawChar(font, x + w - 1, y + iy, BoxDrawing._U_D)
    }
  }

  def drawString(
    x: Int,
    y: Int,
    s: String,
    maxWidth: Int = 0,
    fg: Color = Color.White,
    bg: Color = Color.Black
  ): Unit = {
    for ((c, i) <- s.zipWithIndex) {
      if (maxWidth != 0 && i >= maxWidth) return
      drawChar(font, x + i, y, c, fg, bg)
    }
  }

  def wrapString(maxWidth: Int, maxHeight: Int, s: String): Seq[String] = {
    val lines = mutable.Buffer.empty[String]
    val currentLine = new mutable.StringBuilder()
    for (word <- s.split("\\s")) {
      val wordWithSpace = (if (currentLine.nonEmpty) " " else "") + word
      if (currentLine.size + wordWithSpace.length >= maxWidth) {
        lines.append(currentLine.toString())
        currentLine.clear()
      }
      // TODO: ellipsis
      if (lines.size >= maxHeight) return lines
      if (currentLine.nonEmpty)
        currentLine.append(" ")
      currentLine.append(word)
    }
    if (currentLine.isEmpty)
      lines
    else
      lines :+ currentLine.toString()
  }

  def drawStringWrapped(x: Int, y: Int, maxWidth: Int, maxHeight: Int, s: String): Unit = {
    for ((line, cy) <- wrapString(maxWidth, maxHeight, s).zipWithIndex) {
      drawString(x, y + cy, line)
    }
  }

  def frame(left: Int = 0, top: Int = 0, width: Int = 0, title: String = null, lines: Seq[String]): Unit = {
    drawBox(left, top, width, lines.size + 2)
    if (title != null)
      drawString(left + 1, top, s"[$title]", maxWidth = width-2)
    for ((l, i) <- lines.zipWithIndex) {
      drawString(left + 1, top + 1 + i, l, width - 2)
    }
  }
}
