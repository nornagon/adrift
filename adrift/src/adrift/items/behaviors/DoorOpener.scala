package adrift.items.behaviors

import adrift.GameState
import adrift.items.Message.{Activate, Deactivate, IsOpaque, IsWalkable}
import adrift.items._

case class DoorOpener() extends Behavior {
  var isOpen: Boolean = false

  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Activate if !isOpen =>
      isOpen = true
    case Deactivate =>
      isOpen = false
    case msg: IsOpaque =>
      msg.opaque = !isOpen
    case msg: IsWalkable =>
      msg.walkable = isOpen
    case _ =>
  }
}

