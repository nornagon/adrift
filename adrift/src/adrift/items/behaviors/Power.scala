package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

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
    case _ =>
  }
}

