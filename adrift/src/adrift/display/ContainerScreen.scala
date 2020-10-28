package adrift.display

import adrift.items.Item
import adrift.{Action, GameState, InHands, Inside, ItemLocation, OnFloor, Rect, Worn}
import org.lwjgl.glfw.GLFW._

class ContainerScreen(display: GLFWDisplay, state: GameState, location: ItemLocation) extends Screen {
  import UI.Color._

  class Pane(rootSections: Seq[ItemLocation]) {
    var cursorStack = Seq(0)
    def cursor: Int = cursorStack.head
    def cursor_=(c: Int): Unit = cursorStack = c +: cursorStack.tail

    var sectionStack: Seq[Seq[ItemLocation]] = Seq(rootSections)
    def sections: Seq[ItemLocation] = sectionStack.head

    def items: Seq[Seq[Item]] = sections.map(s => state.items.lookup(s))

    def moveCursor(d: Int): Unit = cursor = math.max(0, math.min(cursor + d, items.map(_.size).sum - 1))

    def moveItem(item: Item): Unit = {
      val toLocation = nonFocusedPane.sections.head
      state.receive(Action.MoveItem(item, toLocation))
    }

    def enterItem(item: Item): Unit = {
      cursorStack = 0 +: cursorStack
      sectionStack = Seq(Inside(item)) +: sectionStack
    }

    def exitItem(): Unit = {
      cursorStack = cursorStack.tail
      sectionStack = sectionStack.tail
    }

    def sectionName(location: ItemLocation): String = location match {
      case OnFloor(l) => "here"
      case InHands() => "hands"
      case Worn() => "worn"
      case Inside(other) => other.kind.name
    }

    def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
      if (action == GLFW_PRESS || action == GLFW_REPEAT) {
        key match {
          case DirectionKey(_, dy) => moveCursor(dy)
          case GLFW_KEY_M =>
            selectedItem.foreach(moveItem)
          case GLFW_KEY_ENTER =>
            selectedItem foreach { i =>
              // TODO: if can contain
              enterItem(i)
            }
          case GLFW_KEY_ESCAPE =>
            if (cursorStack.size > 1) exitItem()
          case _ =>
        }
      }
    }

    def selectedItem: Option[Item] = {
      val is = items.flatten
      if (cursor >= 0 && cursor < is.size) Some(is(cursor))
      else None
    }

    def render(showCursor: Boolean): layout.Box = {
      import layout._
      moveCursor(0)

      val sectionItems = items
      val selectedItem = if (showCursor) this.selectedItem.orNull else null

      vbox(
        children = sections.zipWithIndex.flatMap { case (t, i) =>
          Seq(text(sectionName(t), foreground = disabledGreen)) ++
            (sectionItems(i).map(it =>
              text(" " + state.itemDisplayName(it), background = if (it == selectedItem) selectedGreen else null)
            ) match {
              case Seq() => Seq(text(" (nothing)", foreground = disabledGreen))
              case o => o
            })
        }
      )
    }
  }

  sealed trait CursorSide
  object CursorSide {
    case object Left extends CursorSide
    case object Right extends CursorSide
  }

  private var cursorSide: CursorSide = CursorSide.Left

  private val leftPane = new Pane(Seq(location))
  private val rightPane = new Pane(Seq(InHands(), Worn()))
  private def focusedPane: Pane = cursorSide match {
    case CursorSide.Left => leftPane
    case CursorSide.Right => rightPane
  }
  private def nonFocusedPane: Pane = cursorSide match {
    case CursorSide.Left => rightPane
    case CursorSide.Right => leftPane
  }

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS || action == GLFW_REPEAT) {
      key match {
        case DirectionKey(-1, 0) if cursorSide == CursorSide.Right =>
          cursorSide = CursorSide.Left
        case DirectionKey(1, 0) if cursorSide == CursorSide.Left =>
          cursorSide = CursorSide.Right
        case _ =>
          focusedPane.key(key, scancode, action, mods)
      }
    } else {
      focusedPane.key(key, scancode, action, mods)
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    val paneWidth = 20
    val bounds = Rect.centeredAt(renderer.bounds.center, paneWidth * 2, 40)
    import layout._

    layout.draw(renderer, hbox(
      bounds = bounds,
      background = darkGreen,
      foreground = lightGreen,
      children = Seq(
        vbox(size = 1, fill = '\u00dd'),
        leftPane.render(showCursor = cursorSide == CursorSide.Left),
        vbox(size = 1, fill = '\u00ba', children = Seq(text("\u001d"))),
        rightPane.render(showCursor = cursorSide == CursorSide.Right),
        vbox(size = 1, fill = '\u00de'),
      ))
    )
  }
}
