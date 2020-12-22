package adrift.display

import adrift.items.Message
import adrift.items.behaviors.LayerSet
import adrift.{Color, GameState, Location, OnFloor, Rect}
import layout._

class CableScreen(display: GLFWDisplay, state: GameState) extends Screen {
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {}

  override def render(renderer: GlyphRenderer): Unit = {
    draw(renderer, hbox(
      children = Seq(
        custom(renderCableOverlay),
        renderSidebar()
      ),
      bounds = display.screenRect
    ))
  }

  def frame(contents: Box, size: Int = 0): Box = vbox(
    size = size,
    children = Seq(vbox(
      children = Seq(
        vbox(size = 1),
        hbox(children = Seq(
          vbox(size = 1),
          contents,
          vbox(size = 1)
        )),
        vbox(size = 1)
      )
    )),
    render = (renderer, bounds) => {
      renderer.drawBox(bounds)
    }
  )

  def renderSidebar(): Box = vbox(
    size = 20,
    children = Seq(
      frame(size = 5, contents = vbox(
        children = Seq(text("layers"))
      )),
      frame(contents = vbox(
        children = Seq(text("ports"))
      ))
    )
  )

  def renderCableOverlay(renderer: GlyphRenderer, bounds: Rect): Unit = {
    val levelId = state.player.levelId
    val level = state.levels(levelId)

    for (sy <- bounds.l until bounds.r; sx <- bounds.t until bounds.b; (x, y) = display.screenToWorld(state)((sx, sy)); if level.terrain.contains(x, y)) {
      val layers = level.powerCables(x, y)
      // if there's something with ports here...
      if (state.broadcastToLocation(OnFloor(Location(levelId, x, y)), Message.IsConnected("power-in", LayerSet.all)).connected) {
        // Is it connected to a cable?
        val color = if (layers != 0) Color(1, 0, 0, 1) else Color.White
        renderer.drawChar(bounds.l + sx, bounds.t + sy, 8, fg = color)
      } else {
        if (layers != 0) {
          val connectLeft = level.powerCables.contains(x - 1, y) && ((level.powerCables(x - 1, y) & layers) != 0)
          val connectUp = level.powerCables.contains(x, y - 1) && ((level.powerCables(x, y - 1) & layers) != 0)
          val connectRight = level.powerCables.contains(x + 1, y) && ((level.powerCables(x + 1, y) & layers) != 0)
          val connectDown = level.powerCables.contains(x, y + 1) && ((level.powerCables(x, y + 1) & layers) != 0)
          renderer.drawChar(bounds.l + sx, bounds.t + sy, Appearance.charForConnection(connectLeft, connectUp, connectRight, connectDown), Color(1, 0, 0, 1))
        }
      }
    }

  }
}
