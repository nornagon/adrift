package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

case class Tool(op: String) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case t: Message.UseTool if t.op.id == op =>
      if (state.sendMessage(self, Message.IsFunctional()).functional) {
        t.ok = true
        state.sendMessage(self, Message.ToolUsed(t.op))
      }
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
      t.functional &&= state.broadcastToParts(self, Message.ChargeAvailable(amount = perUse)).ok
    case t: Message.ToolUsed =>
      state.broadcastToParts(self, Message.DrawCharge(amount = perUse))
    case _ =>
  }
}

case class HoldsCharge(maxCharge: Int) extends Behavior {
  var currentCharge: Int = maxCharge

  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case t: Message.DrawCharge =>
      t.ok = currentCharge >= t.amount
      if (t.ok)
        currentCharge -= t.amount
    case t: Message.ChargeAvailable =>
      t.ok ||= currentCharge >= t.amount
    case t: Message.VisibleConditions =>
      t.conditions :+= s"${(currentCharge * 100f / maxCharge).round}% charged"
    case _ =>
  }
}