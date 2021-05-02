package adrift.items.behaviors

import adrift.{GameState, OnFloor}
import adrift.items.{Behavior, Item, Message}

case class Thermostat(targetTemp: Double, hysteresis: Double) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Tick =>
      state.items lookup self match {
        case l @ OnFloor(loc) =>
          val t = state.levels(loc.levelId).temperature(loc.xy)
          if (t < targetTemp - hysteresis) {
            state.broadcastToLocation(l, Message.Activate)
          } else if (t > targetTemp) {
            state.broadcastToLocation(l, Message.Deactivate)
          }
        case _ =>
      }
    case _ =>
  }
}


