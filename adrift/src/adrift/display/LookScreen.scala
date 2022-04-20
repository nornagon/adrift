package adrift.display

import adrift.{Color, GameState, Location, OnFloor}
import org.lwjgl.glfw.GLFW.*

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
    import layout3.*
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

          val w = Border(Column(
            Seq(Text(terrain.name.withFg(UI.Color.lightGreen))) ++
              items.take(9).map(i => Text(state.itemDisplayName(i))) ++
              (if (items.size > 9) Seq(Text(s"${items.size - 9} more...")) else Seq.empty) ++
              (if (state.showGasDebug.nonEmpty) Seq(Text(f"${state.levels(levelId).gasComposition(x, y)} ${state.levels(levelId).gasComposition(x, y).totalPressure}%.1f")) else Seq.empty)
          ))
          draw(
            renderer,
            Offset(anchor._1, anchor._2),
            BoxConstraints.tightFor(width = width),
            w
          )
        case None =>
      }
    } else {
      display.worldToScreen(state)(x, y) match {
        case Some((sx, sy)) =>
          renderer.drawChar(sx, sy, ' ', fg = Color.Black, bg = Color.White)
          if (state.showGasDebug.nonEmpty) {
            val w = Border(Column(
              (if (state.showGasDebug.nonEmpty) Seq(Text(s"${state.levels(levelId).gasComposition(x, y)}")) else Seq.empty)
            ))
            draw(
              renderer,
              Offset(anchor._1, anchor._2),
              BoxConstraints.tightFor(width = width),
              w
            )
          }
        case None =>
      }
    }
  }
}
