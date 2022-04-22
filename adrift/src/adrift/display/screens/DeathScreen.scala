package adrift.display.screens

import adrift.GameState
import adrift.display.{GLFWDisplay, GlyphRenderer, Screen}
import org.lwjgl.glfw.GLFW

class DeathScreen(display: GLFWDisplay, state: GameState) extends Screen {
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    (key, action) match {
      case (GLFW.GLFW_KEY_ESCAPE, GLFW.GLFW_PRESS) => sys.exit(0)
      case _ =>
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    val width = 30
    val height = 20
    val left = display.windowWidthChars / 2 - width / 2
    val top = display.windowHeightChars / 2 - height / 2
    renderer.drawBox(left - 1, top - 1, width + 2, height + 2)
    renderer.drawString(left, top, "You are dead.")
    renderer.drawString(left, top + 2, s"Cause of death: ${state.deathReason.get}")
    renderer.drawString(left, top + 4, "Press [Esc] to quit.")
  }
}
