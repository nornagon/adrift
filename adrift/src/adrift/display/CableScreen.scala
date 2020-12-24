package adrift.display

import adrift.items.Message
import adrift.items.behaviors.LayerSet
import adrift.{Color, GameState, Location, OnFloor, Rect}
import org.lwjgl.glfw.GLFW._
import layout._

object NumericKey {
  def unapply(key: Int): Option[Int] =
    key match {
      case n if n >= GLFW_KEY_0 && n <= GLFW_KEY_9 => Some(n - GLFW_KEY_0)
      case _ => None
    }
}

class CableScreen(display: GLFWDisplay, state: GameState) extends Screen {
  private var displayedPowerLayers = LayerSet.all

  private var cursor = state.player.xy

  val powerColor = Color(1, 0, 0, 1)

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS) {
      key match {
        case DirectionKey(dx, dy) =>
          cursor = (cursor._1 + dx, cursor._2 + dy)
        case GLFW_KEY_SPACE =>
          val cables = state.levels(state.player.levelId).powerCables
          if (cables.contains(cursor)) {
            cables(cursor) ^= displayedPowerLayers.bits
          }
        case NumericKey(n) if n >= 1 && n <= 8 =>
          if ((mods & GLFW_MOD_SHIFT) != 0)
            displayedPowerLayers = displayedPowerLayers.toggle(n - 1)
          else
            displayedPowerLayers = new LayerSet(1 << (n - 1))
        case _ =>
      }
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    draw(renderer, hbox(
      children = Seq(
        custom(renderCableOverlay),
        renderSidebar()
      ),
      bounds = display.screenRect
    ))
  }

  def renderSidebar(): Box = vbox(
    size = 20,
    children = Seq(
      frame(size = 5, contents = vbox(
        children = Seq(
          hbox(
            children = Seq(
              text(" "),
              text(
                (0 until 8).map(i => if (displayedPowerLayers(i)) "\u00fe" else "\u00ff").mkString(""),
                size = 8,
                foreground = powerColor
              ),
              text(" "),
              text("Power", size = 0)
            )
          )
        )
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
      val layers = new LayerSet(level.powerCables(x, y))
      // if there's something with ports here...
      if (state.broadcastToLocation(OnFloor(Location(levelId, x, y)), Message.IsConnected("power-in", LayerSet.all)).connected) {
        // Is it connected to a cable?
        val color = if (layers.intersects(displayedPowerLayers)) powerColor else Color.White
        renderer.drawChar(bounds.l + sx, bounds.t + sy, 8, fg = color)
      } else if (layers.nonEmpty) {
        val connectLeft = level.powerCables.contains(x - 1, y) && ((level.powerCables(x - 1, y) & layers.bits) != 0)
        val connectUp = level.powerCables.contains(x, y - 1) && ((level.powerCables(x, y - 1) & layers.bits) != 0)
        val connectRight = level.powerCables.contains(x + 1, y) && ((level.powerCables(x + 1, y) & layers.bits) != 0)
        val connectDown = level.powerCables.contains(x, y + 1) && ((level.powerCables(x, y + 1) & layers.bits) != 0)
        val color = if (layers.intersects(displayedPowerLayers)) powerColor else Color(0.5f, 0.5f, 0.5f, 1.0f)
        renderer.drawChar(bounds.l + sx, bounds.t + sy, Appearance.charForConnection(connectLeft, connectUp, connectRight, connectDown), color)
      }
    }

    display.worldToScreen(state)(cursor) foreach { cursorScreenPos =>
      renderer.drawChar(cursorScreenPos._1, cursorScreenPos._2, 0xce, UI.Color.lightGreen)
    }
  }
}
