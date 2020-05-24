package adrift.display

import adrift.Color
import adrift.display.glutil.{SpriteBatch, Texture}

import scala.collection.mutable

class GlyphRenderer(
  spriteBatch: SpriteBatch,
  /** how big is a tile in the source texture */
  tileWidth: Int,
  tileHeight: Int,
  /** how big is a tile when drawn to to the screen */
  screenTileWidth: Int,
  screenTileHeight: Int,
  font: Texture,
) {
  /** Number of tiles in each row in the source texture. */
  private val tilesPerRow: Int = font.width / tileWidth
  require(
    font.width / tileWidth.toFloat - tilesPerRow == 0,
    s"tileWidth ($tileWidth) must divide texture width (${font.width}) evenly"
  )

  def drawChar(
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
        font,
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
      font,
      cx * tileWidth, cy * tileHeight,
      tileWidth, tileHeight,
      x * screenTileWidth, y * screenTileHeight,
      screenTileWidth, screenTileHeight,
      fg
    )
  }

  def drawBox(x: Int, y: Int, w: Int, h: Int): Unit = {
    import CP437.BoxDrawing
    drawChar(x, y, BoxDrawing.__RD)
    drawChar(x + w - 1, y, BoxDrawing.L__D)
    drawChar(x, y + h - 1, BoxDrawing._UR_)
    drawChar(x + w - 1, y + h - 1, BoxDrawing.LU__)
    for (iy <- 1 until (h - 1); ix <- 1 until (w - 1))
      drawChar(x + ix, y + iy, ' ')
    for (ix <- 1 until (w - 1)) {
      drawChar(x + ix, y, BoxDrawing.L_R_)
      drawChar(x + ix, y + h - 1, BoxDrawing.L_R_)
    }
    for (iy <- 1 until (h - 1)) {
      drawChar(x, y + iy, BoxDrawing._U_D)
      drawChar(x + w - 1, y + iy, BoxDrawing._U_D)
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
    for ((c, i) <- s.view.zipWithIndex) {
      if (maxWidth != 0 && i >= maxWidth) return
      drawChar(x + i, y, c, fg, bg)
    }
  }

  def drawStringWrapped(x: Int, y: Int, maxWidth: Int, maxHeight: Int, s: String): Unit = {
    for ((line, cy) <- GlyphRenderer.wrapString(maxWidth, maxHeight, s).zipWithIndex) {
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

object GlyphRenderer {
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
      if (lines.size >= maxHeight) return lines.to(Seq)
      if (currentLine.nonEmpty)
        currentLine.append(" ")
      currentLine.append(word)
    }
    if (currentLine.isEmpty)
      if (lines.isEmpty)
        Seq("")
      else
        lines.to(Seq)
    else
      (lines :+ currentLine.toString()).to(Seq)
  }
}
