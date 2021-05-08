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
    type C <: Constraints
    var parentData: ParentData = _
    def layout(constraints: C): Unit
    def paint(glyphRenderer: GlyphRenderer, offset: Offset): Unit
    def applyParentData(): Unit = {}
  }

  case class Pint(i: Int) extends AnyVal with Ordered[Pint] {
    def clamp(lo: Pint, hi: Pint): Pint =
      if (i < lo.i) lo.i
      else if (i > hi.i) hi.i
      else i

    def *(other: Pint): Pint =
      if (i == Int.MaxValue || other.i == Int.MaxValue) Int.MaxValue
      else i * other.i

    def -(other: Pint): Pint =
      if (i == Int.MaxValue) Int.MaxValue
      else if (other.i == Int.MaxValue) 0
      else i - other.i

    def +(other: Pint): Pint =
      if (i == Int.MaxValue) Int.MaxValue
      else i + other.i

    def /(other: Pint): Pint =
      if (i == Int.MaxValue) Int.MaxValue
      else i / other.i

    override def compare(that: Pint): Int = i.compare(that.i)
  }
  implicit def intToPint(i: Int): Pint = Pint(i)

  object pmath {
    def max(a: Pint, b: Pint): Pint = if (a > b) a else b
  }

  case class BoxConstraints(
    minWidth: Pint = 0,
    maxWidth: Pint = Int.MaxValue,
    minHeight: Pint = 0,
    maxHeight: Pint = Int.MaxValue
  ) extends Constraints {
    def enforce(other: BoxConstraints): BoxConstraints = new BoxConstraints(
      minWidth = minWidth.clamp(other.minWidth, other.maxWidth),
      maxWidth = maxWidth.clamp(other.minWidth, other.maxWidth),
      minHeight = minHeight.clamp(other.minHeight, other.maxHeight),
      maxHeight = maxHeight.clamp(other.minHeight, other.maxHeight),
    )

    def constrain(size: Size): Size =
      Size(constrainWidth(size.width).i, constrainHeight(size.height).i)

    def constrainWidth(width: Int = Int.MaxValue): Pint = width.clamp(minWidth, maxWidth)
    def constrainHeight(height: Int = Int.MaxValue): Pint = height.clamp(minHeight, maxHeight)
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
      _lines = GlyphRenderer.wrapCS(text, (constraints.maxWidth * (if (halfWidth) 2 else 1)).i)
      val textSize = Size(
        width = (_lines.view.map(_.length).max + (if (halfWidth) 1 else 0)) / (if (halfWidth) 2 else 1),
        height = _lines.size
      )
      size = constraints.constrain(textSize)
    }

    override def paint(glyphRenderer: GlyphRenderer, offset: Offset): Unit = {
      var y = 0
      for (line <- _lines) {
        if (halfWidth)
          glyphRenderer.drawHalfColoredString(offset.x * 2, offset.y + y, line, bg = Color.Transparent, maxWidth = size.width * 2)
        else
          glyphRenderer.drawColoredString(offset.x, offset.y + y, line, bg = Color.Transparent, maxWidth = size.width)
        y += 1
      }
    }
  }

  sealed trait Axis
  object Axis {
    case object Vertical extends Axis
    case object Horizontal extends Axis
  }

  class FlexParentData extends BoxParentData {
    var flex: Int = 0
  }

  class RenderFlexible(content: RenderBox, flex: Int) extends RenderBox {
    override def layout(constraints: BoxConstraints): Unit = {
      content.layout(constraints)
      size = content.size
    }

    override def paint(glyphRenderer: GlyphRenderer, offset: Offset): Unit = content.paint(glyphRenderer, offset)

    override def applyParentData(): Unit = this.parentData.asInstanceOf[FlexParentData].flex = flex
  }

  class RenderFlex(children: Seq[RenderBox], direction: Axis) extends RenderBox {
    children.foreach { c => c.parentData = new FlexParentData(); c.applyParentData() }

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
      var totalFlex: Int = 0
      val maxMainSize = direction match {
        case Axis.Vertical => constraints.maxHeight
        case Axis.Horizontal => constraints.maxWidth
      }
      val canFlex = maxMainSize < Int.MaxValue
      var lastFlexChild: RenderBox = null
      for (child <- children) {
        val flex = child.parentData.asInstanceOf[FlexParentData].flex
        if (flex > 0) {
          totalFlex += flex
          lastFlexChild = child
        } else {
          val innerConstraints: BoxConstraints = direction match {
            case Axis.Vertical => BoxConstraints(maxWidth = constraints.maxWidth)
            case Axis.Horizontal => BoxConstraints(maxHeight = constraints.maxHeight)
          }
          child.layout(innerConstraints)
          allocatedSize += _getMainSize(child.size)
          crossSize = math.max(crossSize, _getCrossSize(child.size))
        }
      }

      val freeSpace = pmath.max(0, (if (canFlex) maxMainSize else Pint(0)) - allocatedSize)
      var allocatedFlexSpace: Pint = 0

      if (totalFlex > 0) {
        val spacePerFlex: Pint = if (canFlex) freeSpace / totalFlex else -1
        for (child <- children) {
          val flex = child.parentData.asInstanceOf[FlexParentData].flex
          if (flex > 0) {
            val maxChildExtent: Pint = if (canFlex) {
              if (child == lastFlexChild) freeSpace - allocatedFlexSpace else spacePerFlex * flex
            } else Int.MaxValue
            val minChildExtent: Pint = maxChildExtent // TODO: fit
            val innerConstraints = direction match {
              case Axis.Vertical =>
                BoxConstraints(maxWidth = constraints.maxWidth, minHeight = minChildExtent, maxHeight = maxChildExtent)
              case Axis.Horizontal =>
                BoxConstraints(maxHeight = constraints.maxHeight, minWidth = minChildExtent, maxWidth = maxChildExtent)
            }
            child.layout(innerConstraints)
            val childMainSize = _getMainSize(child.size)
            allocatedSize += childMainSize
            allocatedFlexSpace += maxChildExtent
            crossSize = math.max(crossSize, _getCrossSize(child.size));
          }
        }
      }

      var soFar = 0
      for (child <- children) {
        child.parentData.asInstanceOf[BoxParentData].offset = direction match {
          case Axis.Vertical => Offset(0, soFar)
          case Axis.Horizontal => Offset(soFar, 0)
        }
        soFar += _getMainSize(child.size)
      }

      size = constraints.constrain(direction match {
        case Axis.Vertical => Size(crossSize, allocatedSize)
        case Axis.Horizontal => Size(allocatedSize, crossSize)
      })
    }

    override def paint(glyphRenderer: GlyphRenderer, offset: Offset): Unit = {
      for (child <- children)
        child.paint(glyphRenderer, child.parentData.asInstanceOf[BoxParentData].offset + offset)
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
