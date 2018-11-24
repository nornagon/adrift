package adrift.items.behaviors

import adrift.GameState
import adrift.items.Message._
import adrift.items._

case class DoorOpener() extends Behavior {
  var isOpen: Boolean = false

  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Activate if !isOpen =>
      if (state.sendMessage(self, Message.IsFunctional()).functional) {
        isOpen = true
        state.sendMessage(self, Message.ToolUsed(null))
      }
    case Deactivate =>
      if (state.sendMessage(self, Message.IsFunctional()).functional) {
        isOpen = false
        state.sendMessage(self, Message.ToolUsed(null))
      }
    case msg: IsOpaque =>
      msg.opaque = !isOpen
    case msg: IsWalkable =>
      msg.walkable = isOpen
    case msg: Display =>
      msg.display = if (isOpen) "DOOR_OPEN" else "DOOR_CLOSED"
    case _ =>
  }
}

