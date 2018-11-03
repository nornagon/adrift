package adrift.display

import adrift.{Color, GameState, OnFloor}
import org.lwjgl.glfw.GLFW._

class LookScreen(display: GLFWDisplay, state: GameState) extends Screen {
  var (x, y) = state.player
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS || action == GLFW_REPEAT) {
      key match {
        case GLFW_KEY_H | GLFW_KEY_LEFT => x -= 1
        case GLFW_KEY_J | GLFW_KEY_DOWN => y += 1
        case GLFW_KEY_K | GLFW_KEY_UP => y -= 1
        case GLFW_KEY_L | GLFW_KEY_RIGHT => x += 1
        case GLFW_KEY_Y => x -= 1; y -= 1
        case GLFW_KEY_U => x += 1; y -= 1
        case GLFW_KEY_B => x -= 1; y += 1
        case GLFW_KEY_N => x += 1; y += 1
        case _ =>
      }
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    val (char, fg, bg) = Appearance.charAtPosition(state, x, y)
    val (left, right, top, bottom) = display.cameraBounds(state)
    renderer.drawChar(display.font, x - left, y - top, char, fg=Color.Black, bg=Color.White)
    val width = 20
    val anchor =
      if (x - left <= (right - left) / 2 - 1)
        (display.windowWidthChars - 1 - width, 1)
      else
        (1, 1)

    val terrain = state.terrain(x, y)
    val items = state.items.lookup(OnFloor(x, y))

    renderer.frame(
      left = anchor._1, top = anchor._2,
      width = width,
      lines = Seq(terrain.name) ++
        items.take(9).map(_.kind.name) ++
        (if (items.size > 9) Seq(s"${items.size - 9} more...") else Seq.empty)
    )
  }
}
