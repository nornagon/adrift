package adrift.items.behaviors

import adrift.{GameState, Inside}
import adrift.items.Message.{AddCharge, CombustionEnergy, IsCombustible, Tick}
import adrift.items.{Behavior, Item, Message}

case class CombustsContents(rate: Int, var combustingItemJoulesRemaining: Int = 0) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Tick =>
      if (combustingItemJoulesRemaining <= 0) {
        val contents = state.items.lookup(Inside(self))
        for (target <- contents.find(c => state.sendMessage(c, IsCombustible()).ok)) {
          val embodiedEnergy = state.sendMessage(target, CombustionEnergy()).joules
          combustingItemJoulesRemaining += embodiedEnergy
          state.items.delete(target)
        }
      }
      if (combustingItemJoulesRemaining > 0) {
        val d = math.min(combustingItemJoulesRemaining, rate)
        val remaining = state.sendMessage(self, AddCharge(d)).amount
        combustingItemJoulesRemaining -= d - remaining
      }
    case _ =>
  }

  override def tickable: Boolean = true
}

case class Combustible(joules: Int) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: IsCombustible => m.ok = true
    case m: CombustionEnergy => m.joules += joules
    case _ =>
  }
}
