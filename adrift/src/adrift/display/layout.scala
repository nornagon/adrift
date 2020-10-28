package adrift.display

import adrift.{Color, Rect}

import scala.collection.mutable

object layout {
  trait Direction
  case object Vertical extends Direction
  case object Horizontal extends Direction

  case class Box(
    direction: Direction,
    bounds: Option[Rect],
    size: Int,
    background: Option[Color],
    foreground: Option[Color],
    fill: Option[Char],
    children: Seq[Box],
    text: Option[String]
  )

  def hbox(
    bounds: Rect = null,
    background: Color = null,
    foreground: Color = null,
    fill: Char = ' ',
    size: Int = 0,
    children: Seq[Box] = Seq.empty
  ): Box = Box(
    direction = Horizontal,
    bounds = Option(bounds),
    background = Option(background),
    foreground = Option(foreground),
    fill = Option(fill),
    size = size,
    children = children,
    text = None,
  )

  def vbox(
    bounds: Rect = null,
    background: Color = null,
    foreground: Color = null,
    fill: Char = ' ',
    size: Int = 0,
    children: Seq[Box] = Seq.empty
  ): Box = Box(
    direction = Vertical,
    bounds = Option(bounds),
    background = Option(background),
    foreground = Option(foreground),
    fill = Option(fill),
    size = size,
    children = children,
    text = None,
  )

  def text(
    text: String,
    background: Color = null,
    foreground: Color = null
  ): Box = Box(
    direction = Horizontal,
    bounds = None,
    background = Option(background),
    foreground = Option(foreground),
    fill = None,
    size = 1,
    children = Seq.empty,
    text = Some(text),
  )

  def draw(renderer: GlyphRenderer, box: Box): Unit = {
    drawIn(box, box.bounds.getOrElse(throw new RuntimeException("root needs bounds")), Color.White, Color.Black)

    def drawIn(box: Box, bounds: Rect, parentFg: Color, parentBg: Color): Unit = {
      val currentFg = box.foreground.getOrElse(parentFg)
      val currentBg = box.background.getOrElse(parentBg)
      // 1. fill background
      if (box.background.nonEmpty || box.fill.exists(_ != ' '))
        renderer.fillRect(bounds, box.fill.getOrElse(' '), bg = box.background.getOrElse(currentBg), fg = box.foreground.getOrElse(currentFg))
      // 2. draw content
      // 2a. if there's text, draw it
      for (text <- box.text)
        renderer.drawString(bounds.l, bounds.t, text, maxWidth = bounds.width, fg = currentFg, bg = currentBg)
      // 2b. if there are children, lay them out and draw them
      if (box.children.nonEmpty) {
        val availableSize = box.direction match {
          case Horizontal => bounds.width
          case Vertical => bounds.height
        }
        val specifiedSize = box.children.view.map(_.size).sum
        val remainingAfterSpecified = availableSize - specifiedSize
        val numUnspecifiedChildren = box.children.count(_.size == 0)
        val minimumSizePerUnspecifiedChild = if (numUnspecifiedChildren != 0) remainingAfterSpecified / numUnspecifiedChildren else 0
        val extraCells = remainingAfterSpecified - minimumSizePerUnspecifiedChild * numUnspecifiedChildren
        val childSizes = mutable.Seq.fill[Int](box.children.size)(0)
        var remainingExtraCells = extraCells
        for ((child, i) <- box.children.zipWithIndex) {
          child.size match {
            case 0 =>
              childSizes(i) = minimumSizePerUnspecifiedChild + (if (remainingExtraCells > 0) {
                remainingExtraCells -= 1
                1
              } else 0)
            case s =>
              childSizes(i) = s
          }
        }
        val splitPoints = childSizes.scanLeft(0)(_ + _)
        val childBounds = box.direction match {
          case Horizontal => bounds.cutHorizontal(splitPoints)
          case Vertical => bounds.cutVertical(splitPoints)
        }
        for ((child, bounds) <- box.children.zip(childBounds)) {
          drawIn(child, bounds, currentFg, currentBg)
        }
      }
    }
  }
}
