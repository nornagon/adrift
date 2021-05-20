package adrift.items.behaviors

import adrift.items.Message.IsPermeable
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
      val level = state.levels(loc.levelId)
      val gc = level.gasComposition(loc.xy)
      level.setGasComposition(loc.x, loc.y, gc + GasComposition(oxygen = 1, carbonDioxide = 0, nitrogen = 0))
    case _ =>
  }
}

case class HoleInTheWall() extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Tick =>
      val loc = state.getItemTile(self)
      val level = state.levels(loc.levelId)
      level.setGasComposition(loc.x, loc.y, GasComposition(oxygen = 0, carbonDioxide = 0, nitrogen = 0))
      state.markPermeabilityDirty(loc)
    case msg: IsPermeable =>
      msg.permeable = true
    case _ =>
  }
}