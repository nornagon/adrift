package adrift.display

import adrift._
import adrift.items.{Broken, Item}
import org.lwjgl.glfw.GLFW._

class InventoryScreen(display: GLFWDisplay, state: GameState) extends Screen {
  var selectedIdx = 0
  def selectedItem: Option[Item] = {
    val nearbyItems = state.nearbyItems
    if (selectedIdx >= 0 && selectedIdx < nearbyItems.size)
      Some(nearbyItems(selectedIdx))
    else None
  }

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    val nearbyItems = state.nearbyItems
    if (action == GLFW_PRESS || action == GLFW_REPEAT)
      key match {
        case GLFW_KEY_J | GLFW_KEY_DOWN => selectedIdx = (selectedIdx + 1) % nearbyItems.size
        case GLFW_KEY_K | GLFW_KEY_UP => selectedIdx = (selectedIdx + nearbyItems.size - 1) % nearbyItems.size
        case GLFW_KEY_E =>
          if (selectedIdx >= 0 && selectedIdx < nearbyItems.size)
            display.pushScreen(new ExamineScreen(display, state, nearbyItems(selectedIdx)))
        case GLFW_KEY_G =>
          if (selectedIdx >= 0 && selectedIdx < nearbyItems.size)
            display.pushAction(Action.PickUp(nearbyItems(selectedIdx)))
        case GLFW_KEY_D =>
          if (selectedIdx >= 0 && selectedIdx < nearbyItems.size && state.items.lookup(nearbyItems(selectedIdx)).isInstanceOf[InHands])
            display.pushAction(Action.PutDown(nearbyItems(selectedIdx)))
        case _ =>
      }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    val nearbyItems = state.nearbyItems
    val anchor = (1, 1)
    val width = 30
    selectedItem foreach { item =>
      val loc = state.items.lookup(item)
      val (left, _, top, _) = display.cameraBounds(state)
      loc match {
        case OnFloor(x, y) =>
          val (char, _, _) = Appearance.charForItem(state, item)
          renderer.drawChar(display.font, x - left, y - top, char, fg=Color.Black, bg=Color.White)
        case InHands() | Worn() =>
          val (char, _, _) = Appearance.charForItem(state, item)
          renderer.drawChar(display.font, state.player._1 - left, state.player._2 - top, char, fg=Color.Black, bg=Color.White)
        case Inside(otherItem) =>
          // TODO: recursively find the location of otherItem until something is worn, in hands, or on the floor.
      }
    }
    renderer.drawBox(anchor._1, anchor._2, width, anchor._2 + 2 + nearbyItems.size)
    renderer.drawString(anchor._1 + 1, anchor._2 + 1, "Nearby")
    for ((item, i) <- nearbyItems.zipWithIndex) {
      val pos = state.items.lookup(item)
      var name = item.kind.name
      if (item.conditions.contains(Broken))
        name += " (broken)"
      renderer.drawString(anchor._1 + 2, anchor._2 + 2 + i, name, maxWidth = width - 3 - 2)
      renderer.drawString(width - 2, anchor._2 + 2 + i, directionString(pos))
    }
    if (nearbyItems.nonEmpty)
      renderer.drawString(anchor._1 + 1, anchor._2 + 2 + selectedIdx, ">")
  }

  def directionString(pos: ItemLocation): String = {
    pos match {
      case OnFloor(x, y) =>
        val dx = x - state.player._1
        val dy = y - state.player._2
        if (dx < 0) {
          if (dy < 0) "NW"
          else if (dy == 0) "W"
          else "SW"
        } else if (dx == 0) {
          if (dy < 0) "N"
          else if (dy == 0) "."
          else "S"
        } else {
          if (dy < 0) "NE"
          else if (dy == 0) "E"
          else "SE"
        }
      case InHands() =>
        "H"
      case Inside(other) =>
        directionString(state.items.lookup(other))
      case Worn() =>
        "W"
    }
  }
}
