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

case class ProvidesInfinitePower() extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: Message.ChargeAvailable => m.ok = true
    case m: Message.DrawCharge =>
    case _ =>
  }
}

case class UsesElectricity(perUse: Int) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case t: Message.IsFunctional =>
      t.functional &&= state.sendMessage(self, Message.ChargeAvailable(amount = perUse)).ok
    case t: Message.Conditions =>
      if (!state.sendMessage(self, Message.ChargeAvailable(amount = perUse)).ok)
        t.conditions :+= "unpowered"
    case t: Message.ToolUsed =>
      state.sendMessage(self, Message.DrawCharge(amount = perUse))
    case t: Message.ChargeAvailable =>
      state.broadcastToParts(self, t)
    case t: Message.DrawCharge =>
      state.broadcastToParts(self, t)
    case _ =>
  }
}

