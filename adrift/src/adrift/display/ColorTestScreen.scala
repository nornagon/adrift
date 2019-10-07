package adrift.display

import adrift.GameState
import org.lwjgl.glfw.GLFW

class ColorTestScreen(display: GLFWDisplay, state: GameState) extends Screen {
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = (action, key) match {
    case (GLFW.GLFW_PRESS, GLFW.GLFW_KEY_ESCAPE) =>
      display.popScreen()
    case _ =>
  }

  override def render(renderer: GlyphRenderer): Unit = {
    val palette = state.data.display.palette
    val colorNames = palette.keySet.toList.sorted
    val centerX = display.windowWidthChars / 2
    val width = palette.size + colorNames.maxBy(_.length).length + 1
    val left = centerX - width / 2 - 1
    val top = 2
    renderer.drawBox(left, top, width + 2, palette.size + 2)
    for ((fg, y) <- colorNames.zipWithIndex; (bg, x) <- colorNames.zipWithIndex) {
      renderer.drawString(left + x + 1, top + y + 1, "x", fg = palette(fg), bg = palette(bg))
    }
    for ((fg, y) <- colorNames.zipWithIndex) {
      renderer.drawString(left + 1 + palette.size + 1, top + y + 1, fg, fg = palette(fg))
    }
  }
}
