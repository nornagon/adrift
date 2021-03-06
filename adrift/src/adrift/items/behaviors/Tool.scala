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
      if (state.isFunctional(self)) {
        t.ok = true
        state.sendMessage(self, Message.ToolUsed(t.op))
      }
    case t: Message.Provides if t.op.id == op =>
      t.provides = true
    case _ =>
  }
}

case class HoldsCharge(maxCharge: Int, var currentCharge: Int = -1) extends Behavior {
  if (currentCharge == -1) currentCharge = maxCharge
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case t: Message.AddCharge =>
      val spaceAvailable = maxCharge - currentCharge
      val d = math.min(t.amount, spaceAvailable)
      t.amount -= d
      currentCharge += d
    case t: Message.DrawCharge =>
      t.ok = currentCharge >= t.amount
      if (t.ok)
        currentCharge -= t.amount
    case t: Message.ChargeAvailable =>
      t.ok ||= currentCharge >= t.amount
    case t: Message.Conditions =>
      t.conditions :+= s"${(currentCharge * 100f / maxCharge).round}% charged"
    case _ =>
  }
}
