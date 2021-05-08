package adrift.display

import adrift.{Color, Rect}
import adrift.display.CP437.BoxDrawing
import adrift.display.GlyphRenderer.ColoredString

import scala.language.implicitConversions

// Inspired by Flutter
object layout3 {
  // There are 3 trees: Widget, Element and RenderObject
  // Each tree controls the next one: Widget => Element => RenderObject
  // Widgets are the immutable logical structure. A Widget is a _description of_ an Element.
  // Elements are the instantiated persistent structure.
  // RenderObjects are the realized physical structure. This is where layout happens.

  // Example Widget tree:
  // Border(
  //   VStack(
  //     Text(...),
  //     Text(...),
  //     Text(...),
  //   )
  // )

  case class Size(width: Int, height: Int) {
    def +(size: Size): Size = Size(width + size.width, height + size.height)
  }
  case class Offset(x: Int, y: Int) {
    def +(offset: Offset): Offset = Offset(x + offset.x, y + offset.y)
  }

  trait Widget

  trait Element

  trait Constraints
  trait ParentData
  abstract class RenderObject {
    type PD <: ParentData
    type C <: Constraints
    var parentData: PD = _
    def layout(constraints: C): Unit
    def paint(glyphRenderer: GlyphRenderer, offset: Offset): Unit
  }

  case class BoxConstraints(
    minWidth: Int = 0,
    maxWidth: Int = Int.MaxValue,
    minHeight: Int = 0,
    maxHeight: Int = Int.MaxValue
  ) extends Constraints {
    def enforce(other: BoxConstraints): BoxConstraints = new BoxConstraints(
      minWidth = math.max(other.minWidth, math.min(other.maxWidth, minWidth)),
      maxWidth = math.max(other.minWidth, math.min(other.maxWidth, maxWidth)),
      minHeight = math.max(other.minHeight, math.min(other.maxHeight, minHeight)),
      maxHeight = math.max(other.minHeight, math.min(other.maxHeight, maxHeight)),
    )

    def constrain(size: Size): Size =
      Size(constrainWidth(size.width), constrainHeight(size.height));

    def constrainWidth(width: Int = Int.MaxValue): Int =
      math.max(minWidth, math.min(maxWidth, width))

    def constrainHeight(height: Int = Int.MaxValue): Int =
      math.max(minHeight, math.min(maxHeight, height))
  }
  object BoxConstraints {
    def tight(size: Size): BoxConstraints = new BoxConstraints(size.width, size.width, size.height, size.height)
    def loose(size: Size): BoxConstraints = new BoxConstraints(0, size.width, 0, size.height)
  }

  class BoxParentData extends ParentData {
    var offset: Offset = Offset(0, 0)
  }

  abstract class RenderBox extends RenderObject {
    type PD = BoxParentData
    type C = BoxConstraints
    var _size: Size = _
    def size: Size = _size
    protected def size_=(size: Size): Unit = _size = size
  }

  class RenderBorder(content: RenderBox) extends RenderBox {
    override def layout(constraints: BoxConstraints): Unit = {
      content.layout(constraints.copy(maxWidth = constraints.maxWidth - 2, maxHeight = constraints.maxHeight - 2))
      size = content.size + Size(2, 2)
    }

    override def paint(glyphRenderer: GlyphRenderer, offset: Offset): Unit = {
      glyphRenderer.drawBox(offset.x, offset.y, size.width, size.height)
      content.paint(glyphRenderer, offset + Offset(1, 1))
    }
  }

  class RenderLRBorder(content: RenderBox, fg: Color, bg: Color) extends RenderBox {
    override def layout(constraints: BoxConstraints): Unit = {
      content.layout(constraints.copy(maxWidth = constraints.maxWidth - 2))
      size = content.size + Size(2, 0)
    }

    override def paint(glyphRenderer: GlyphRenderer, offset: Offset): Unit = {
      for (y <- offset.y until offset.y + size.height) {
        glyphRenderer.drawChar(offset.x, y, 0xdd, fg, bg)
        glyphRenderer.drawChar(offset.x + size.width - 1, y, 0xde, fg, bg)
      }
      content.paint(glyphRenderer, offset + Offset(1, 0))
    }
  }

  implicit def convertStringToColoredString(string: String): ColoredString = ColoredString(string)
  class RenderText(text: ColoredString, halfWidth: Boolean = true) extends RenderBox {
    private var _lines: Seq[ColoredString] = Seq.empty
    override def layout(constraints: BoxConstraints): Unit = {
      _lines = GlyphRenderer.wrapCS(text, constraints.maxWidth * (if (halfWidth) 2 else 1))
      val textSize = Size(
        width = _lines.view.map(_.length).max / (if (halfWidth) 2 else 1),
        height = _lines.size
      )
      size = constraints.constrain(textSize)
    }

    override def paint(glyphRenderer: GlyphRenderer, offset: Offset): Unit = {
      var y = 0
      for (line <- _lines) {
        if (halfWidth)
          glyphRenderer.drawHalfColoredString(offset.x * 2, offset.y + y, line, bg = Color.Transparent)
        else
          glyphRenderer.drawColoredString(offset.x, offset.y + y, line, bg = Color.Transparent)
        y += 1
      }
    }
  }

  sealed trait Axis
  object Axis {
    case object Vertical extends Axis
    case object Horizontal extends Axis
  }

  class RenderFlex(children: Seq[RenderBox], direction: Axis) extends RenderBox {
    children.foreach { c => c.parentData = new BoxParentData() }

    private def _getMainSize(size: Size): Int = direction match {
      case Axis.Vertical => size.height
      case Axis.Horizontal => size.width
    }

    private def _getCrossSize(size: Size): Int = direction match {
      case Axis.Vertical => size.width
      case Axis.Horizontal => size.height
    }

    override def layout(constraints: BoxConstraints): Unit = {
      var allocatedSize: Int = 0
      var crossSize: Int = 0
      for (child <- children) {
        val innerConstraints: BoxConstraints = direction match {
          case Axis.Vertical => BoxConstraints(maxWidth = constraints.maxWidth)
          case Axis.Horizontal => BoxConstraints(maxHeight = constraints.maxHeight)
        }
        child.layout(innerConstraints)
        child.parentData.offset = direction match {
          case Axis.Vertical => Offset(0, allocatedSize)
          case Axis.Horizontal => Offset(allocatedSize, 0)
        }
        allocatedSize += _getMainSize(child.size)
        crossSize = math.max(crossSize, _getCrossSize(child.size))
      }
      size = constraints.constrain(direction match {
        case Axis.Vertical => Size(crossSize, allocatedSize)
        case Axis.Horizontal => Size(allocatedSize, crossSize)
      })
    }

    override def paint(glyphRenderer: GlyphRenderer, offset: Offset): Unit = {
      for (child <- children)
        child.paint(glyphRenderer, child.parentData.offset + offset)
    }
  }

  class RenderConstrainedBox(additionalConstraints: BoxConstraints, content: RenderBox = null) extends RenderBox {
    override def layout(constraints: BoxConstraints): Unit = {
      if (content != null) {
        content.layout(additionalConstraints.enforce(constraints))
        size = content.size
      } else {
        size = additionalConstraints.enforce(constraints).constrain(Size(0, 0))
      }
    }

    override def paint(glyphRenderer: GlyphRenderer, offset: Offset): Unit =
      if (content != null) content.paint(glyphRenderer, offset)
  }

  class RenderBackground(content: RenderBox, bg: Color) extends RenderBox {
    override def layout(constraints: BoxConstraints): Unit = {
      content.layout(constraints)
      size = content.size
    }

    override def paint(glyphRenderer: GlyphRenderer, offset: Offset): Unit = {
      glyphRenderer.fillRect(Rect(l = offset.x, t = offset.y, r = offset.x + size.width, b = offset.y + size.height), ' ', bg = bg, fg = Color.Transparent)
      content.paint(glyphRenderer, offset)
    }
  }
}
