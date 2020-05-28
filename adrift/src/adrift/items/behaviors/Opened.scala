package adrift.items.behaviors

import adrift.GameState
import adrift.items.Message.{IsFunctional, IsOpened}
import adrift.items.{Behavior, Item, Message}

case class Opened() extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: IsOpened => m.opened = true
    case m: IsFunctional => m.functional = false
    case _ =>
  }
}
