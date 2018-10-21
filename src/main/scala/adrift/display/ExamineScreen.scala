package adrift.display

import adrift.items.Item
import adrift.GameState
import org.lwjgl.glfw.GLFW._

class ExamineScreen(display: GLFWDisplay, state: GameState, item: Item) extends Screen {
  private val anchor = (5, 3)
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS) {
      key match {
        case GLFW_KEY_D if (mods & GLFW_MOD_SHIFT) != 0 =>
          if (item.parts.nonEmpty)
            display.pushScreen(new DisassembleScreen(display, state, item))
        case _ =>
      }
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    val itemsByKind = item.parts.groupBy(_.kind)
    val width = 30
    val descriptionLines = renderer.wrapString(maxWidth = width - 2, maxHeight = 9, item.kind.description)
    renderer.frame(
      left = anchor._1, top = anchor._2,
      width = width,
      title = item.kind.name,
      lines = descriptionLines ++
        Seq("", "Parts:") ++
        itemsByKind.map {
          case (kind, items) if items.size == 1 => kind.name
          case (kind, items) => s"${items.size} x ${kind.name}"
        }
    )
  }
}
