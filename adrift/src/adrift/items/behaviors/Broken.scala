package adrift.items.behaviors

import adrift.GameState
import adrift.items.Message.{IsFunctional, VisibleConditions}
import adrift.items.{Behavior, Item, Message}

case class Broken() extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = {
    message match {
      case t: IsFunctional =>
        t.functional = false
      case t: VisibleConditions =>
        t.conditions :+= "broken"
      case _ =>
    }
  }
}
