package adrift.display

import adrift.{Color, GameState, Location, OnFloor}
import org.lwjgl.glfw.GLFW._

class LookScreen(display: GLFWDisplay, state: GameState) extends Screen {
  var Location(levelId, x, y) = state.player

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS || action == GLFW_REPEAT) {
      key match {
        case DirectionKey(dx, dy) =>
          x += dx
          y += dy
        case _ =>
      }
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    val loc = Location(state.player.levelId, x, y)
    val isVisible = state.isVisible(loc)
    val width = 20
    val anchor = (1, 1)
    if (isVisible && state.levels(levelId).terrain.contains(x, y)) {
      val (char, fg, bg) = Appearance.charAtPosition(state, x, y)
      display.worldToScreen(state)(x, y) match {
        case Some((sx, sy)) =>
          renderer.drawChar(sx, sy, char, fg = Color.Black, bg = Color.White)

          val terrain = state.levels(levelId).terrain(x, y)
          val items = state.items.lookup(OnFloor(Location(levelId, x, y)))

          renderer.frame(
            left = anchor._1, top = anchor._2,
            width = width,
            halfWidth = true,
            lines = Seq(terrain.name) ++
              items.take(9).map(state.itemDisplayName) ++
              (if (items.size > 9) Seq(s"${items.size - 9} more...") else Seq.empty)
          )
        case None =>
      }
    } else {
      display.worldToScreen(state)(x, y) match {
        case Some((sx, sy)) =>
          renderer.drawChar(sx, sy, ' ', fg = Color.Black, bg = Color.White)
        case None =>
      }
    }
  }
}
