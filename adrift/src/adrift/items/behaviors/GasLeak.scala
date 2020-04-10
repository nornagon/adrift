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
      val loc = state.getItemTile(self)
      state.levels(loc.levelId).gasComposition(loc.x, loc.y) += GasComposition(oxygen = 1, carbonDioxide = 0, nitrogen = 0)
    case _ =>
  }
}
