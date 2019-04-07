package adrift.display

import adrift.GameState
import org.lwjgl.glfw.GLFW

class WishScreen(display: GLFWDisplay, state: GameState) extends Screen {
  var input = ""
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW.GLFW_PRESS || action == GLFW.GLFW_REPEAT) {
      key match {
        case GLFW.GLFW_KEY_BACKSPACE if input.length > 0 =>
          input = input.init
        case GLFW.GLFW_KEY_ENTER =>
          input match {
            case "wtw" =>
              state.walkThroughWalls = !state.walkThroughWalls
            case "showtemp" =>
              state.showTempDebug = !state.showTempDebug
            case _ =>
          }
          display.popScreen()
        case _ =>
      }
    }
  }

  override def char(char: Int): Unit = {
    input += char.toChar
  }

  override def render(renderer: GlyphRenderer): Unit = {
    renderer.frame(2, 2, 40, "Wish", Seq(""))
    renderer.drawString(3, 3, input, maxWidth = 38)
    renderer.drawString(3 + input.length, 3, "\u00db")
  }
}
