package adrift.items.behaviors

import adrift.GameState
import adrift.items.Message.IsPermeable
import adrift.items.{Behavior, Item, Message}

case class Permeable() extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case p: IsPermeable =>
      p.permeable = true
    case _ =>
  }
}
