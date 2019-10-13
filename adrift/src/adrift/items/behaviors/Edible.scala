package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

case class Edible(calories: Int) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: Message.Eat =>
      m.calories += calories
      state.items.delete(self)
    case m: Message.IsEdible =>
      m.edible = true
    case _ =>
  }
}
