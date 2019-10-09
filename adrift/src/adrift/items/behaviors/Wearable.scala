package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

case class Wearable(location: Seq[String], warmth: Double) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: Message.CanWear =>
      m.ok = true
    case m: Message.LoseHeat =>
      m.dq *= (1 - warmth)
    case _ =>
  }
}
