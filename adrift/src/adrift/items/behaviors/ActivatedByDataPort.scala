package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

case class ActivatedByDataPort(portName: String) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.ReceivedDataPacket(port, value) if port == portName =>
      if (value == 0)
        state.sendMessage(self, Message.Deactivate)
      else
        state.sendMessage(self, Message.Activate)
    case _ =>
  }
}
