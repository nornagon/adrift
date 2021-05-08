package adrift.display

import adrift.Rect
import adrift.display.GlyphRenderer.ColoredString

import scala.language.implicitConversions

// inspired by SwiftUI
object layout2 {
  case class ProposedSize(width: Option[Int], height: Option[Int])
  case class Size(width: Int, height: Int)

  implicit def convertStringToColoredString(string: String): ColoredString = ColoredString(string)

  abstract class View {
    var offset: (Int, Int) = (0, 0)
    var size: Size = Size(0, 0)
    def layout(proposedSize: ProposedSize): Unit
    def draw(glyphRenderer: GlyphRenderer, anchor: (Int, Int)): Unit
  }

  class Border(content: View) extends View {
    override def layout(proposedSize: ProposedSize): Unit = {
      val maxChildSize = ProposedSize(
        proposedSize.width.map(w => (w - 2).max(0)),
        proposedSize.height.map(h => (h - 2).max(0))
      )
      content.layout(maxChildSize)
      this.size = Size(content.size.width + 2, content.size.height + 2)
      content.offset = (1, 1)
    }

    override def draw(glyphRenderer: GlyphRenderer, anchor: (Int, Int)): Unit = {
      glyphRenderer.drawBox(anchor._1, anchor._2, size.width, size.height)
      content.draw(glyphRenderer, (anchor._1 + 1, anchor._2 + 1))
    }
  }

  class Text(text: ColoredString, halfWidth: Boolean = true) extends View {
    private def divisor: Int = if (halfWidth) 2 else 1
    override def layout(proposedSize: ProposedSize): Unit = {
      this.size = proposedSize.width match {
        case None =>
          Size((text.length + divisor / 2) / divisor, 1)
        case Some(maxWidth) =>
          val lines = GlyphRenderer.wrapCS(text, maxWidth * divisor)
          Size(
            lines.map(l => (l.length + divisor / 2) / divisor).max,
            math.min(lines.length, proposedSize.height.getOrElse(Int.MaxValue))
          )
      }
    }

    override def draw(glyphRenderer: GlyphRenderer, anchor: (Int, Int)): Unit = {
      val lines = GlyphRenderer.wrapCS(text, size.width * divisor).take(size.height)
      for ((line, y) <- lines.zipWithIndex) {
        if (halfWidth)
          glyphRenderer.drawHalfColoredString(anchor._1 * 2, anchor._2 + y, line)
        else
          glyphRenderer.drawColoredString(anchor._1, anchor._2 + y, line)
      }
    }
  }

  class Frame(content: View, width: Option[Int] = None, maxWidth: Option[Int] = None, height: Option[Int] = None, maxHeight: Option[Int] = None) extends View {
    override def layout(proposedSize: ProposedSize): Unit = {
      val childProposedWidth =
        if (width.nonEmpty) width
        else if (maxWidth.nonEmpty) Some(math.min(proposedSize.width.getOrElse(Int.MaxValue), maxWidth.get))
        else proposedSize.width
      val childProposedHeight =
        if (height.nonEmpty) height
        else if (maxHeight.nonEmpty) Some(math.min(proposedSize.height.getOrElse(Int.MaxValue), maxHeight.get))
        else proposedSize.height
      content.layout(ProposedSize(childProposedWidth, childProposedHeight))
      size = Size(width.getOrElse(content.size.width), height.getOrElse(content.size.height))
    }

    override def draw(glyphRenderer: GlyphRenderer, anchor: (Int, Int)): Unit = content.draw(glyphRenderer, anchor)
  }

  class VStack(children: Seq[View]) extends View {
    override def layout(proposedSize: ProposedSize): Unit = {
      proposedSize.height match {
        case Some(height) =>
          // split the proposed height between our children
          var remainingChildren = children
          var remainingHeight = height
          size = Size(0, 0)
          while (remainingChildren.nonEmpty) {
            val remainingHeightPerChild = remainingHeight / remainingChildren.size
            val child = remainingChildren.head
            child.layout(ProposedSize(proposedSize.width, Some(remainingHeightPerChild)))
            child.offset = (0, height - remainingHeight)
            remainingChildren = remainingChildren.tail
            remainingHeight -= child.size.height
            size = Size(width = size.width.max(child.size.width), height = size.height + child.size.height)
          }
        case None =>
          var y = 0
          size = Size(0, 0)
          for (child <- children) {
            child.layout(proposedSize)
            child.offset = (0, y)
            y += child.size.height
            size = Size(width = size.width.max(child.size.width), height = size.height + child.size.height)
          }
      }
    }

    override def draw(glyphRenderer: GlyphRenderer, anchor: (Int, Int)): Unit = {
      for (child <- children) {
        val childAnchor = (child.offset._1 + anchor._1, child.offset._2 + anchor._2)
        child.draw(glyphRenderer, childAnchor)
      }
    }
  }

  class HStack(children: Seq[View]) extends View {
    override def layout(proposedSize: ProposedSize): Unit = {
      proposedSize.width match {
        case Some(width) =>
          // split the proposed width between our children
          var remainingChildren = children
          var remainingWidth = width
          size = Size(0, 0)
          while (remainingChildren.nonEmpty) {
            val remainingWidthPerChild = remainingWidth / remainingChildren.size
            val child = remainingChildren.head
            child.layout(ProposedSize(Some(remainingWidthPerChild), proposedSize.height))
            child.offset = (width - remainingWidth, 0)
            remainingChildren = remainingChildren.tail
            remainingWidth -= child.size.width
            size = Size(width = size.width + child.size.width, height = size.height.max(child.size.height))
          }
        case None =>
          var x = 0
          size = Size(0, 0)
          for (child <- children) {
            child.layout(proposedSize)
            child.offset = (x, 0)
            x += child.size.width
            size = Size(width = size.width + child.size.width, height = size.height.max(child.size.height))
          }
      }
    }

    override def draw(glyphRenderer: GlyphRenderer, anchor: (Int, Int)): Unit = {
      for (child <- children) {
        val childAnchor = (child.offset._1 + anchor._1, child.offset._2 + anchor._2)
        child.draw(glyphRenderer, childAnchor)
      }
    }
  }

  def draw(renderer: GlyphRenderer, bounds: Rect, view: View): Unit = {
    view.layout(ProposedSize(Some(bounds.width), Some(bounds.height)))
    view.draw(renderer, (bounds.l + view.offset._1, bounds.t + view.offset._2))
  }
}
