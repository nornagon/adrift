package adrift.items.behaviors

import adrift.GameState
import adrift.items.Message.{Conditions, IsFunctional}
import adrift.items.{Behavior, Item, Message}

case class PartInstalled() extends Message

case class MissingParts() extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: IsFunctional => m.functional = false
    case m: PartInstalled =>
      if (self.missingParts.isEmpty)
        self.behaviors -= this
    case m: Conditions =>
      m.conditions :+= "missing parts"
    case _ =>
  }
}
