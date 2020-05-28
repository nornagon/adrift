package adrift.display

import adrift.{Color, Rect}
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
  val bounds: Rect,
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

  def drawBox(rect: Rect, fg: Color = Color.White, bg: Color = Color.Black): Unit =
    drawBox(rect.l, rect.t, rect.width, rect.height, fg, bg)
  def drawBox(x: Int, y: Int, w: Int, h: Int): Unit = drawBox(x, y, w, h, Color.White, Color.Black)
  def drawBox(x: Int, y: Int, w: Int, h: Int, fg: Color, bg: Color): Unit = {
    import CP437.BoxDrawing
    drawChar(x, y, BoxDrawing.__RD, fg, bg)
    drawChar(x + w - 1, y, BoxDrawing.L__D, fg, bg)
    drawChar(x, y + h - 1, BoxDrawing._UR_, fg, bg)
    drawChar(x + w - 1, y + h - 1, BoxDrawing.LU__, fg, bg)
    for (iy <- 1 until (h - 1); ix <- 1 until (w - 1))
      drawChar(x + ix, y + iy, ' ', fg, bg)
    for (ix <- 1 until (w - 1)) {
      drawChar(x + ix, y, BoxDrawing.L_R_, fg, bg)
      drawChar(x + ix, y + h - 1, BoxDrawing.L_R_, fg, bg)
    }
    for (iy <- 1 until (h - 1)) {
      drawChar(x, y + iy, BoxDrawing._U_D, fg, bg)
      drawChar(x + w - 1, y + iy, BoxDrawing._U_D, fg, bg)
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

  case class Ann(from: Int, until: Int, fg: Color) {
    require(from <= until, s"annotation must have from <= until but was ($from, $until)")
    def intersects(from: Int, until: Int): Boolean = this.until > from && this.from < until
    def intersection(from: Int, until: Int): Option[Ann] = {
      if (intersects(from, until)) {
        Some(Ann(math.max(from, this.from), math.min(until, this.until), fg))
      } else None
    }

    def +(x: Int): Ann = copy(from = from + x, until = until + x)
    def -(x: Int): Ann = copy(from = from - x, until = until - x)
  }
  case class CS(s: String, as: Seq[Ann]) {
    def parts: Seq[(String, Seq[Ann])] =
      for (i <- s.indices) yield (String.valueOf(s(i)), as.filter(_.intersects(i, i + 1)))

    def substring(start: Int, end: Int): CS =
      CS(s.substring(start, end), as.flatMap(_.intersection(start, end)).map(_ - start))

    def length: Int = s.length

    def splitAt(i: Int): (CS, CS) = {
      val (l, r) = s.splitAt(i)
      (CS(l, as.flatMap(_.intersection(0, l.length))), CS(r, as.flatMap(_.intersection(l.length, s.length))))
    }

    def +(other: CS): CS =
      CS(s + other.s, as ++ other.as.map(_ + s.length))
  }
  object CS { def empty: CS = CS("", Seq.empty) }

  // Adapted from https://github.com/apache/commons-lang/blob/9747b14/src/main/java/org/apache/commons/lang3/text/WordUtils.java#L274
  // Copyright 2001-2020 The Apache Software Foundation
  def wrap(str: String, wrapLength: Int = 1, wrapLongWords: Boolean = true, wrapOn: String = " "): Seq[String] = {
    import java.util.regex.Pattern
    import scala.util.control.Breaks._
    if (str == null) return Seq.empty
    val patternToWrapOn = Pattern.compile(wrapOn)
    val inputLineLength = str.length
    var offset = 0
    var wrappedLine = Seq.empty[String]
    def append(string: String, start: Int, end: Int): Unit = {
      if (wrappedLine.isEmpty)
        wrappedLine = Seq("")
      wrappedLine = wrappedLine.init :+ (wrappedLine.last + str.substring(start, end))
    }
    breakable {
      while (offset < inputLineLength) {
        var spaceToWrapAt = -1
        var matcher = patternToWrapOn.matcher(str.substring(offset, Math.min(Math.min(Integer.MAX_VALUE, offset + wrapLength + 1L).toInt, inputLineLength)))
        var shouldContinue = false
        if (matcher.find) {
          if (matcher.start == 0) {
            offset += matcher.end
            shouldContinue = true
          } else {
            spaceToWrapAt = matcher.start + offset
          }
        }
        if (!shouldContinue) {
          // only last line without leading spaces is left
          if (inputLineLength - offset <= wrapLength) break
          while (matcher.find) spaceToWrapAt = matcher.start + offset
          if (spaceToWrapAt >= offset) { // normal case
            append(str, offset, spaceToWrapAt)
            wrappedLine :+= ""
            offset = spaceToWrapAt + 1
          } else { // really long word or URL
            if (wrapLongWords) { // wrap really long word one line at a time
              append(str, offset, wrapLength + offset)
              wrappedLine :+= ""
              offset += wrapLength
            } else { // do not wrap really long word, just extend beyond limit
              matcher = patternToWrapOn.matcher(str.substring(offset + wrapLength))
              if (matcher.find) spaceToWrapAt = matcher.start + offset + wrapLength
              if (spaceToWrapAt >= 0) {
                append(str, offset, spaceToWrapAt)
                wrappedLine :+= ""
                offset = spaceToWrapAt + 1
              } else {
                append(str, offset, str.length)
                offset = inputLineLength
              }
            }
          }
        }
      }
    }
    // Whatever is left in line is short enough to just pass through
    append(str, offset, str.length)
    wrappedLine
  }

  def wrapCS(str: CS, wrapLength: Int = 1, wrapLongWords: Boolean = true, wrapOn: String = " "): Seq[CS] = {
    import java.util.regex.Pattern
    import scala.util.control.Breaks._
    if (str == null) return Seq.empty
    val patternToWrapOn = Pattern.compile(wrapOn)
    val inputLineLength = str.length
    var offset = 0
    var wrappedLine = Seq.empty[CS]
    def append(string: CS, start: Int, end: Int): Unit = {
      if (wrappedLine.isEmpty)
        wrappedLine = Seq(CS.empty)
      wrappedLine = wrappedLine.init :+ (wrappedLine.last + str.substring(start, end))
    }
    breakable {
      while (offset < inputLineLength) {
        var spaceToWrapAt = -1
        var matcher = patternToWrapOn.matcher(str.s.substring(offset, Math.min(Math.min(Integer.MAX_VALUE, offset + wrapLength + 1L).toInt, inputLineLength)))
        var shouldContinue = false
        if (matcher.find) {
          if (matcher.start == 0) {
            offset += matcher.end
            shouldContinue = true
          } else {
            spaceToWrapAt = matcher.start + offset
          }
        }
        if (!shouldContinue) {
          // only last line without leading spaces is left
          if (inputLineLength - offset <= wrapLength) break
          while (matcher.find) spaceToWrapAt = matcher.start + offset
          if (spaceToWrapAt >= offset) { // normal case
            append(str, offset, spaceToWrapAt)
            wrappedLine :+= CS.empty
            offset = spaceToWrapAt + 1
          } else { // really long word or URL
            if (wrapLongWords) { // wrap really long word one line at a time
              append(str, offset, wrapLength + offset)
              wrappedLine :+= CS.empty
              offset += wrapLength
            } else { // do not wrap really long word, just extend beyond limit
              matcher = patternToWrapOn.matcher(str.s.substring(offset + wrapLength))
              if (matcher.find) spaceToWrapAt = matcher.start + offset + wrapLength
              if (spaceToWrapAt >= 0) {
                append(str, offset, spaceToWrapAt)
                wrappedLine :+= CS.empty
                offset = spaceToWrapAt + 1
              } else {
                append(str, offset, str.length)
                offset = inputLineLength
              }
            }
          }
        }
      }
    }
    // Whatever is left in line is short enough to just pass through
    append(str, offset, str.length)
    wrappedLine
  }
}
