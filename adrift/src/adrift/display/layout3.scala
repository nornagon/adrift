package adrift.display

import adrift.display.GlyphRenderer.ColoredString
import adrift.{Color, Rect}

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

  // TODO:
  trait Widget {
    // TODO: all the updating and stuff
    def inflate: RenderObject
  }
  // trait Element

  case class Flex(
    direction: Axis,
    children: Seq[Widget],
    mainAxisAlignment: MainAxisAlignment = MainAxisAlignment.Start,
    crossAxisAlignment: CrossAxisAlignment = CrossAxisAlignment.Start,
    verticalDirection: VerticalDirection = VerticalDirection.Down,
    clipBehavior: ClipBehavior = ClipBehavior.None,
  ) extends Widget {
    override def inflate: RenderObject = new RenderFlex(
      direction = direction,
      mainAxisAlignment = mainAxisAlignment,
      crossAxisAlignment = crossAxisAlignment,
      verticalDirection = verticalDirection,
      clipBehavior = clipBehavior,
      children = children.map(_.inflate.asInstanceOf[RenderBox])
    )
  }
  case class Row(
    children: Seq[Widget],
    mainAxisAlignment: MainAxisAlignment = MainAxisAlignment.Start,
    crossAxisAlignment: CrossAxisAlignment = CrossAxisAlignment.Start,
    verticalDirection: VerticalDirection = VerticalDirection.Down,
    clipBehavior: ClipBehavior = ClipBehavior.None,
  ) extends Widget {
    override def inflate: RenderObject = new RenderFlex(
      direction = Axis.Horizontal,
      mainAxisAlignment = mainAxisAlignment,
      crossAxisAlignment = crossAxisAlignment,
      verticalDirection = verticalDirection,
      clipBehavior = clipBehavior,
      children = children.map(_.inflate.asInstanceOf[RenderBox])
    )
  }
  case class Column(
    children: Seq[Widget],
    mainAxisAlignment: MainAxisAlignment = MainAxisAlignment.Start,
    crossAxisAlignment: CrossAxisAlignment = CrossAxisAlignment.Start,
    verticalDirection: VerticalDirection = VerticalDirection.Down,
    clipBehavior: ClipBehavior = ClipBehavior.None,
  ) extends Widget {
    override def inflate: RenderObject = new RenderFlex(
      direction = Axis.Vertical,
      mainAxisAlignment = mainAxisAlignment,
      crossAxisAlignment = crossAxisAlignment,
      verticalDirection = verticalDirection,
      clipBehavior = clipBehavior,
      children = children.map(_.inflate.asInstanceOf[RenderBox])
    )
  }
  case class Text(
    text: ColoredString,
    halfWidth: Boolean = true
  ) extends Widget {
    override def inflate: RenderObject = new RenderText(text, halfWidth)
  }

  case class Border(
    content: Widget
  ) extends Widget {
    override def inflate: RenderObject = new RenderBorder(content.inflate.asInstanceOf[RenderBox])
  }

  case class LRBorder(
    fg: Color,
    bg: Color,
    content: Widget
  ) extends Widget {
    override def inflate: RenderObject = new RenderLRBorder(content.inflate.asInstanceOf[RenderBox], fg, bg)
  }

  case class Flexible(
    content: Widget,
    flex: Int = 1,
  ) extends Widget {
    override def inflate: RenderObject = {
      val child = content.inflate.asInstanceOf[RenderBox]
      val pd = new FlexParentData()
      pd.flex = flex
      child.parentData = pd
      child
    }
  }

  case class Spacer(flex: Int = 1) extends Widget {
    override def inflate: RenderObject =
      Flexible(ConstrainedBox(BoxConstraints.tight(Size(0, 0)))).inflate
  }

  case class ConstrainedBox(
    additionalConstraints: BoxConstraints,
    content: Widget = null,
  ) extends Widget {
    override def inflate: RenderObject = new RenderConstrainedBox(
      additionalConstraints,
      Option(content).map(_.inflate.asInstanceOf[RenderBox]).orNull
    )
  }

  case class Background(
    bg: Color,
    content: Widget
  ) extends Widget {
    override def inflate: RenderObject = new RenderBackground(content.inflate.asInstanceOf[RenderBox], bg)
  }

  trait Constraints
  trait ParentData
  abstract class RenderObject {
    type C <: Constraints
    var parentData: ParentData = _
    def layout(constraints: C): Unit
    def paint(glyphRenderer: GlyphRenderer, offset: Offset): Unit
    def applyParentData(): Unit = {}
  }

  // Int but Int.MaxValue works like Double.PositiveInfinity
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

    override def toString: String = if (i == Int.MaxValue) "∞" else i.toString
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

    override def toString: String = s"BoxConstraints(width ∈ [$minWidth,$maxWidth], height ∈ [$minHeight,$maxHeight])"
  }
  object BoxConstraints {
    def tight(size: Size): BoxConstraints = new BoxConstraints(size.width, size.width, size.height, size.height)
    def loose(size: Size): BoxConstraints = new BoxConstraints(0, size.width, 0, size.height)
  }

  class BoxParentData extends ParentData {
    var offset: Offset = Offset(0, 0)
  }

  abstract class RenderBox extends RenderObject {
    type C = BoxConstraints
    var _size: Size = _
    def size: Size = _size
    protected def size_=(size: Size): Unit = _size = size
  }

  class RenderBorder(content: RenderBox) extends RenderBox {
    override def layout(constraints: BoxConstraints): Unit = {
      content.layout(new BoxConstraints(
        minWidth = pmath.max(0, constraints.minWidth - 2),
        maxWidth = constraints.maxWidth - 2,
        minHeight = pmath.max(0, constraints.minHeight - 2),
        maxHeight = constraints.maxHeight - 2
      ))
      size = content.size + Size(2, 2)
    }

    override def paint(glyphRenderer: GlyphRenderer, offset: Offset): Unit = {
      glyphRenderer.drawBox(offset.x, offset.y, size.width, size.height)
      content.paint(glyphRenderer, offset + Offset(1, 1))
    }
  }

  class RenderLRBorder(content: RenderBox, fg: Color, bg: Color) extends RenderBox {
    override def layout(constraints: BoxConstraints): Unit = {
      content.layout(constraints.copy(
        minWidth = pmath.max(0, constraints.minWidth - 2),
        maxWidth = constraints.maxWidth - 2,
      ))
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
      if (constraints.maxHeight <= 1) {
        val clipped =
          if (text.length <= (constraints.maxWidth * (if (halfWidth) 2 else 1)).i) text
          else text.substring(0, (constraints.maxWidth * (if (halfWidth) 2 else 1) - 1).i) + "\u0010"
        _lines = Seq(clipped)
      } else {
        _lines = GlyphRenderer.wrapCS(text, (constraints.maxWidth * (if (halfWidth) 2 else 1)).i)
      }
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

    override def toString: String = s"RenderText(${text.plain})"
  }

  sealed trait Axis
  object Axis {
    case object Vertical extends Axis
    case object Horizontal extends Axis
  }

  sealed trait MainAxisAlignment
  object MainAxisAlignment {
    case object Start extends MainAxisAlignment
    case object End extends MainAxisAlignment
  }

  sealed trait CrossAxisAlignment
  object CrossAxisAlignment {
    case object Start extends CrossAxisAlignment
    case object End extends CrossAxisAlignment
    case object Stretch extends CrossAxisAlignment
  }

  sealed trait VerticalDirection
  object VerticalDirection {
    case object Down extends VerticalDirection
    case object Up extends VerticalDirection
  }

  sealed trait ClipBehavior
  object ClipBehavior {
    case object Clip extends ClipBehavior
    case object None extends ClipBehavior
  }

  class FlexParentData extends BoxParentData {
    var flex: Int = 0
  }

  class RenderFlex(
    children: Seq[RenderBox],
    direction: Axis,
    mainAxisAlignment: MainAxisAlignment = MainAxisAlignment.Start,
    crossAxisAlignment: CrossAxisAlignment = CrossAxisAlignment.Start,
    verticalDirection: VerticalDirection = VerticalDirection.Down,
    clipBehavior: ClipBehavior = ClipBehavior.None,
  ) extends RenderBox {
    children.foreach { c =>
      if (!c.parentData.isInstanceOf[FlexParentData])
        c.parentData = new FlexParentData()
      c.applyParentData()
    }

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
      var lastFlexChild: RenderObject = null
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
            val innerConstraints = crossAxisAlignment match {
              case CrossAxisAlignment.Stretch =>
                direction match {
                  case Axis.Vertical =>
                    BoxConstraints(minWidth = constraints.maxWidth, maxWidth = constraints.maxWidth)
                  case Axis.Horizontal =>
                    BoxConstraints(minHeight = constraints.maxHeight, maxHeight = constraints.maxHeight)
                }
              case _ =>
                direction match {
                  case Axis.Vertical =>
                    BoxConstraints(maxWidth = constraints.maxWidth, minHeight = minChildExtent, maxHeight = maxChildExtent)
                  case Axis.Horizontal =>
                    BoxConstraints(maxHeight = constraints.maxHeight, minWidth = minChildExtent, maxWidth = maxChildExtent)
                }
            }

            child.layout(innerConstraints)
            val childMainSize = _getMainSize(child.size)
            allocatedSize += childMainSize
            allocatedFlexSpace += maxChildExtent
            crossSize = math.max(crossSize, _getCrossSize(child.size));
          }
        }
      }

      // Align items along the main axis
      size = constraints.constrain(direction match {
        case Axis.Vertical => Size(crossSize, allocatedSize)
        case Axis.Horizontal => Size(allocatedSize, crossSize)
      })
      val actualSize = direction match {
        case Axis.Vertical => size.height
        case Axis.Horizontal => size.width
      }

      val actualSizeDelta = actualSize - allocatedSize
      val remainingSpace = math.max(0, actualSizeDelta)

      def _startIsTopLeft(axis: Axis, direction: VerticalDirection) = {
        axis match {
          case Axis.Horizontal => true // TODO: rtl
          case Axis.Vertical => direction match {
            case VerticalDirection.Down => true
            case VerticalDirection.Up => false
          }
        }
      }

      def flipAxis(axis: Axis): Axis = axis match {
        case Axis.Vertical => Axis.Horizontal
        case Axis.Horizontal => Axis.Vertical
      }

      val flipMainAxis = !_startIsTopLeft(direction, verticalDirection)
      val leadingSpace = mainAxisAlignment match {
        case MainAxisAlignment.Start => 0
        case MainAxisAlignment.End => remainingSpace
      }

      var childMainPosition = if (flipMainAxis) actualSize - leadingSpace else leadingSpace
      for (child <- children) {
        val childCrossPosition = crossAxisAlignment match {
          case CrossAxisAlignment.Start | CrossAxisAlignment.End =>
            if (_startIsTopLeft(flipAxis(direction), verticalDirection) == (crossAxisAlignment == CrossAxisAlignment.Start))
              0
            else
              crossSize - _getCrossSize(child.size)
          case CrossAxisAlignment.Stretch => 0
        }
        if (flipMainAxis) childMainPosition -= _getMainSize(child.size)
        child.parentData.asInstanceOf[BoxParentData].offset = direction match {
          case Axis.Vertical => Offset(childCrossPosition, childMainPosition)
          case Axis.Horizontal => Offset(childMainPosition, childCrossPosition)
        }
        if (flipMainAxis) {
          // TODO: childMainPosition -= betweenSpace
        } else {
          childMainPosition += _getMainSize(child.size)
        }
      }

    }

    override def paint(glyphRenderer: GlyphRenderer, offset: Offset): Unit = {
      val childRenderer = if (clipBehavior == ClipBehavior.None) {
        glyphRenderer
      } else {
        glyphRenderer.clip(Rect(offset.x, offset.y, offset.x + size.width, offset.y + size.height))
      }
      for (child <- children)
        child.paint(childRenderer, child.parentData.asInstanceOf[BoxParentData].offset + offset)
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
