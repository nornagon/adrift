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
    val width = 30
    val descriptionLines = renderer.wrapString(maxWidth = width - 2, maxHeight = 9, item.kind.description)
    renderer.frame(
      left = anchor._1, top = anchor._2,
      width = width,
      title = item.kind.name,
      lines = descriptionLines ++
        (if (item.parts.nonEmpty)
          Seq("", "Parts:") ++
          item.parts.groupBy(state.itemDisplayName).map {
            case (displayName, items) if items.size == 1 => displayName
            case (displayName, items) => s"${items.size} x $displayName"
          }
        else Seq.empty)
    )
  }
}
