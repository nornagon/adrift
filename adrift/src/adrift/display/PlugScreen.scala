package adrift.display

import adrift.items.{Item, Message}
import adrift.{Action, GameState, OnFloor}
import org.lwjgl.glfw.GLFW._

class PlugScreen(display: GLFWDisplay, state: GameState, plugee: Item) extends Screen {
  var selectedIdx = 0

  val pluggableItems = (state.items.lookup(OnFloor(state.player)) ++
    Seq((0, -1), (-1, 0), (1, 0), (0, 1)).flatMap {
      case (dx, dy) => state.items.lookup(OnFloor(state.player))
    })
    .filter(_ != plugee)
    .filter(i => state.sendMessage(plugee, Message.CanPlugInto(i)).ok)

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS || action == GLFW_REPEAT) {
      key match {
        case GLFW_KEY_J | GLFW_KEY_DOWN => selectedIdx = (selectedIdx + 1) % pluggableItems.size
        case GLFW_KEY_K | GLFW_KEY_UP => selectedIdx = (selectedIdx + pluggableItems.size - 1) % pluggableItems.size
        case GLFW_KEY_ENTER =>
          if (selectedIdx >= 0 && selectedIdx < pluggableItems.size) {
            display.pushAction(Action.Plug(plugee, pluggableItems(selectedIdx)))
            display.popScreen()
            display.popScreen()
          }
        case _ =>
      }
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    renderer.frame(
      width = 40,
      title = "Plug into what?",
      lines =
        pluggableItems.zipWithIndex.map { case (item, i) =>
          val selector = if (i == selectedIdx) ">" else " "
          s"$selector ${state.itemDisplayName(item)}"
        }
    )
  }
}
