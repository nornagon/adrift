package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

class Block extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: Message.IsWalkable => m.walkable = false
    case _ =>
  }
}
