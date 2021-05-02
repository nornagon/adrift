package adrift.items.behaviors

import adrift.items.{Behavior, Item, Message}
import adrift.{GameState, OnFloor}

case class Heater(var active: Boolean = true, dq: Double = 0.5) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Activate =>
      active = true
    case Message.Deactivate =>
      active = false
    case Message.Tick if active =>
      state.items lookup self match {
        case OnFloor(loc) =>
          state.levels(loc.levelId).temperature(loc.xy) += dq / state.levels(loc.levelId).terrain(loc.xy).heatCapacity
        case _ =>
      }
    case _ =>
  }
}
