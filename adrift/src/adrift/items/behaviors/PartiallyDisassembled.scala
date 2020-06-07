package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

case class PartiallyDisassembled() extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case c: Message.Conditions => c.conditions :+= "partially disassembled"
    case c: Message.IsFunctional => c.functional = false
    case _ =>
  }
}
