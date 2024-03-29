package adrift.display

import adrift.display.GlyphRenderer.ColoredString
import adrift.display.glutil.{SpriteBatch, Texture}
import adrift.{Color, Rect}

import scala.collection.mutable

case class GlyphRenderer(
  spriteBatch: SpriteBatch,
  /** how big is a tile in the source texture */
  tileWidth: Int,
  tileHeight: Int,
  /** how big is a tile when drawn to to the screen */
  screenTileWidth: Int,
  screenTileHeight: Int,
  font: Texture,
  bounds: Rect,
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
    if (!bounds.contains(x, y)) return
    if (bg.a != 0) {
      // 0xdb is the full white square ◻️
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

  def drawParticle(x: Float, y: Float, size: Float, color: Color = Color.White): Unit = {
    if (!bounds.contains(x.toInt, y.floor.toInt)) return
    // 0xdb is the full white square ◻️
    val cx = 0xdb % tilesPerRow
    val cy = 0xdb / tilesPerRow
    spriteBatch.drawRegion(
      font,
      (cx * tileWidth).toFloat, (cy * tileHeight).toFloat,
      tileWidth.toFloat, tileHeight.toFloat,
      x * screenTileWidth - size / 2, y * screenTileHeight - size / 2,
      size, size,
      color
    )
  }

  def drawHalfChar(
    halfX: Int,
    y: Int,
    c: Int,
    fg: Color = Color.White,
    bg: Color = Color.Black
  ): Unit = {
    if (!bounds.contains(halfX / 2, y)) return
    if (bg.a != 0) {
      // 0xdb is the full white square ◻️
      val cx = 0xdb % tilesPerRow
      val cy = 0xdb / tilesPerRow
      spriteBatch.drawRegion(
        font,
        cx * tileWidth, cy * tileHeight,
        tileWidth, tileHeight,
        halfX * screenTileWidth / 2, y * screenTileHeight,
        screenTileWidth / 2, screenTileHeight,
        bg
      )
    }
    val cx = c % tilesPerRow
    val cy = c / tilesPerRow
    spriteBatch.drawRegion(
      font,
      cx * tileWidth, cy * tileHeight,
      tileWidth, tileHeight,
      halfX * screenTileWidth / 2, y * screenTileHeight,
      screenTileWidth / 2, screenTileHeight,
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

  def fillRect(bounds: Rect, c: Char, bg: Color, fg: Color): Unit = {
    for (y <- bounds.t until bounds.b; x <- bounds.l until bounds.r)
      drawChar(x, y, c, bg = bg, fg = fg)
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

  def drawHalfString(
    halfX: Int,
    y: Int,
    s: String,
    maxWidth: Int = 0,
    fg: Color = Color.White,
    bg: Color = Color.Black
  ): Unit = {
    for ((c, i) <- s.view.zipWithIndex) {
      if (maxWidth != 0 && i >= maxWidth) return
      drawHalfChar(halfX + i, y, c, fg, bg)
    }
  }

  def drawColoredString(
    x: Int,
    y: Int,
    cs: ColoredString,
    maxWidth: Int = 0,
    fg: Color = Color.White,
    bg: Color = Color.Black,
  ): Unit = {
    var widthRemaining = maxWidth
    var tx = x
    for ((s, anns) <- cs.parts) {
      val color = anns.lastOption.map(_.data).getOrElse(fg)
      drawString(tx, y, s, fg = color, bg = bg, maxWidth = widthRemaining)
      tx += s.length
      if (maxWidth != 0) {
        widthRemaining -= s.length
        if (widthRemaining <= 0)
          return
      }
    }
  }

  def drawHalfColoredString(
    halfX: Int,
    y: Int,
    cs: ColoredString,
    maxWidth: Int = -1,
    fg: Color = Color.White,
    bg: Color = Color.Black,
  ): Unit = {
    var widthRemaining = maxWidth
    var tx = halfX
    for ((s, anns) <- cs.parts) {
      val color = anns.lastOption.map(_.data).getOrElse(fg)
      drawHalfString(tx, y, s, fg = color, bg = bg, maxWidth = widthRemaining)
      tx += s.length
      if (maxWidth >= 0) {
        widthRemaining -= s.length
        if (widthRemaining <= 0)
          return
      }
    }
  }

  def frame(left: Int = 0, top: Int = 0, width: Int = 0, title: String = null, halfWidth: Boolean = false, lines: Seq[String]): Unit = {
    drawBox(left, top, width, lines.size + 2)
    if (title != null)
      drawString(left + 1, top, s"[$title]", maxWidth = width-2)
    for ((l, i) <- lines.zipWithIndex) {
      if (halfWidth)
        drawHalfString((left + 1) * 2, top + 1 + i, l, (width - 2) * 2)
      else
        drawString(left + 1, top + 1 + i, l, width - 2)
    }
  }

  def clip(rect: Rect): GlyphRenderer = copy(bounds = (rect & bounds).getOrElse(Rect(0, 0, 0, 0)))
}

object GlyphRenderer {
  def wrapString(maxWidth: Int, maxHeight: Int, s: String): Seq[String] =
    wrap(s, maxWidth).take(maxHeight)

  case class Ann[T](from: Int, until: Int, data: T) {
    require(from <= until, s"annotation must have from <= until but was ($from, $until)")
    def intersects(from: Int, until: Int): Boolean = this.until > from && this.from < until
    def intersection(from: Int, until: Int): Option[Ann[T]] = {
      if (intersects(from, until)) {
        Some(Ann(math.max(from, this.from), math.min(until, this.until), data))
      } else None
    }

    def +(x: Int): Ann[T] = copy(from = from + x, until = until + x)
    def -(x: Int): Ann[T] = copy(from = from - x, until = until - x)
  }

  case class AnnotatedString[T](s: String, as: Seq[Ann[T]] = Seq.empty) {
    def parts: Seq[(String, Seq[Ann[T]])] =
      for (i <- s.indices) yield (String.valueOf(s(i)), as.filter(_.intersects(i, i + 1)))

    def substring(start: Int, end: Int): AnnotatedString[T] =
      AnnotatedString(s.substring(start, end), as.flatMap(_.intersection(start, end)).map(_ - start))

    def length: Int = s.length

    def splitAt(i: Int): (AnnotatedString[T], AnnotatedString[T]) =
      (substring(0, i), substring(i, s.length))

    def +(other: AnnotatedString[T]): AnnotatedString[T] =
      AnnotatedString(s + other.s, as ++ other.as.map(_ + s.length))

    def ann(from: Int, until: Int, data: T): AnnotatedString[T] = copy(as = as :+ Ann(from, until, data))

    def withAnn(data: T): AnnotatedString[T] = copy(as = Seq(Ann(0, s.length, data)))

    def asColoredString: AnnotatedString[T] = this

    def plain: String = parts.view.map(_._1).mkString("")
  }

  object AnnotatedString {
    def empty[T]: AnnotatedString[T] = AnnotatedString("", Seq.empty)
  }

  type ColoredString = AnnotatedString[Color]
  object ColoredString {
    def empty: ColoredString = AnnotatedString.empty
  }

  implicit class AnnotatedColorString(s: AnnotatedString[Color]) {
    def withFg(c: Color): ColoredString = s.withAnn(c)
  }

  // Adapted from https://github.com/apache/commons-lang/blob/9747b14/src/main/java/org/apache/commons/lang3/text/WordUtils.java#L274
  // Copyright 2001-2020 The Apache Software Foundation
  def wrap(str: String, wrapLength: Int = 1, wrapLongWords: Boolean = true, wrapOn: String = " "): Seq[String] = {
    import java.util.regex.Pattern
    import scala.util.control.Breaks.*
    if (str == null) return Seq.empty
    val patternToWrapOn = Pattern.compile(wrapOn)
    val inputLineLength = str.length
    var offset = 0
    var wrappedLine = Seq.empty[String]
    def append(string: String, start: Int, end: Int): Unit = {
      if (wrappedLine.isEmpty)
        wrappedLine = Seq("")
      wrappedLine = wrappedLine.init :+ (wrappedLine.last + string.substring(start, end))
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
          if (inputLineLength - offset <= wrapLength) break()
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

  def wrapCS(str: ColoredString, wrapLength: Int = 1, wrapLongWords: Boolean = true, wrapOn: String = " "): Seq[ColoredString] = {
    var s = str
    val lines = mutable.Buffer.empty[ColoredString]
    while (s.length > 0) {
      val nextNewline = s.s.indexOf("\n")
      if (nextNewline >= 0) {
        val line = s.substring(0, nextNewline)
        val rest = s.substring(nextNewline + 1, s.length)
        lines.append(line)
        s = rest
      } else {
        lines.append(s)
        s = ColoredString.empty
      }
    }
    lines.toSeq.flatMap(this.wrapSingleLineCS(_, wrapLength, wrapLongWords, wrapOn)) match {
      case Seq() => Seq(ColoredString.empty)
      case x => x
    }
  }

  /** Text-wrap a colored string so it fits in |wrapLength|. */
  def wrapSingleLineCS(str: ColoredString, wrapLength: Int = 1, wrapLongWords: Boolean = true, wrapOn: String = " "): Seq[ColoredString] = {
    import java.util.regex.Pattern
    import scala.util.control.Breaks.*
    if (str == null) return Seq.empty
    if (str.length == 0) return Seq(str)
    if (wrapLength >= str.length) return Seq(str)
    val patternToWrapOn = Pattern.compile(wrapOn)
    val inputLineLength = str.length
    var offset = 0
    var wrappedLine = Seq.empty[ColoredString]
    def append(string: ColoredString, start: Int, end: Int): Unit = {
      if (wrappedLine.isEmpty)
        wrappedLine = Seq(ColoredString.empty)
      wrappedLine = wrappedLine.init :+ (wrappedLine.last + string.substring(start, end))
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
          if (inputLineLength - offset <= wrapLength) break()
          while (matcher.find) spaceToWrapAt = matcher.start + offset
          if (spaceToWrapAt >= offset) { // normal case
            append(str, offset, spaceToWrapAt)
            wrappedLine :+= ColoredString.empty
            offset = spaceToWrapAt + 1
          } else { // really long word or URL
            if (wrapLongWords) { // wrap really long word one line at a time
              append(str, offset, wrapLength + offset)
              wrappedLine :+= ColoredString.empty
              offset += wrapLength
            } else { // do not wrap really long word, just extend beyond limit
              matcher = patternToWrapOn.matcher(str.s.substring(offset + wrapLength))
              if (matcher.find) spaceToWrapAt = matcher.start + offset + wrapLength
              if (spaceToWrapAt >= 0) {
                append(str, offset, spaceToWrapAt)
                wrappedLine :+= ColoredString.empty
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
