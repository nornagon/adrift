package adrift.items.behaviors

import adrift.GameState
import adrift.items.Message.CanContainItems
import adrift.items.{Behavior, Item, Message}

case class Container() extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: CanContainItems => m.ok = true
    case _ =>
  }
}
