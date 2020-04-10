package adrift.items.behaviors

import adrift.GameState
import adrift.items.Message._
import adrift.items._

case class DoorOpener(
  var isOpen: Boolean = false
) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Activate if !isOpen =>
      if (state.isFunctional(self)) {
        isOpen = true
        state.sendMessage(self, Message.ToolUsed(null))
        state.markPermeabilityDirty(state.getItemTile(self))
      }
    case Deactivate =>
      if (state.isFunctional(self)) {
        isOpen = false
        state.sendMessage(self, Message.ToolUsed(null))
        state.markPermeabilityDirty(state.getItemTile(self))
      }
    case msg: IsOpaque =>
      msg.opaque = !isOpen
    case msg: IsWalkable =>
      msg.walkable = isOpen
    case msg: IsPermeable =>
      msg.permeable = isOpen
    case msg: Display =>
      msg.display = if (isOpen) "DOOR_OPEN" else "DOOR_CLOSED"
    case _ =>
  }
}

