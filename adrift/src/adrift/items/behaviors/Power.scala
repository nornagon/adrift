package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

case class ProvidesShipPower(circuit: String) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: Message.ChargeAvailable =>
      if (state.isFunctional(self) && state.circuits(circuit).stored >= m.amount) {
        m.ok = true
      }
    case m: Message.DrawCharge =>
      if (state.isFunctional(self))
        state.circuits(circuit).stored -= m.amount
    case _ =>
  }
}

case class GeneratesShipPower(circuit: String, amountPerTick: Int) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Tick =>
      if (state.isFunctional(self))
        state.circuits(circuit).add(amountPerTick)
      // TODO: use fuel
    case _ =>
  }
}
