package adrift.display

import adrift.{Action, GameState}
import adrift.items.Item
import org.lwjgl.glfw.GLFW._

class AssemblyScreen(display: GLFWDisplay, state: GameState) extends Screen {
  val buildable = state.buildableItems(state.nearbyItems)

  var selectedIdx = 0

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS || action == GLFW_REPEAT)
      key match {
        case GLFW_KEY_J | GLFW_KEY_DOWN => selectedIdx = (selectedIdx + 1) % buildable.size
        case GLFW_KEY_K | GLFW_KEY_UP => selectedIdx = (selectedIdx + buildable.size - 1) % buildable.size
        case GLFW_KEY_ENTER =>

          val (kind, locations) = buildable(selectedIdx)

          display.pushAction(
            Action.Assemble(kind, locations)
          )
        case _ =>
      }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    renderer.frame(
      left = 1,
      top = 1,
      width = 40,
      title = "Assemble what?",
      lines =
        buildable.zipWithIndex.map { case ((k, _), i) =>
          val selector = if (i == selectedIdx) "> " else "  "
          s"$selector${Item.item_display(k.display).toChar} ${k.name}"
        }
    )
  }
}
