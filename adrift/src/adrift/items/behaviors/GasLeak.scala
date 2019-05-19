package adrift.items.behaviors

import adrift.{GameState, GasComposition, OnFloor}
import adrift.items.{Behavior, Item, Message}

case class GasLeak() extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Tick =>
      state.gasComposition(state.items.lookup(self) match {
        case OnFloor(x, y) => (x, y)
      }) += GasComposition(oxygen = 1, carbonDioxide = 0, nitrogen = 0)
    case _ =>
  }
}
