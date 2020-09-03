package adrift.items.behaviors

import adrift.GameState
import adrift.items.Message.{CanPickUp, PickUp}
import adrift.items.{Behavior, Item, Message}

case class Affixed() extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case p: PickUp => p.ok = false
    case p: CanPickUp => p.ok = false
    case _ =>
  }
}

