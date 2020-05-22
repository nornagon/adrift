package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

case class DisplaysConnectedTo(terrainTypes: Seq[String]) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m @ Message.DisplayConnectedTo(terrain, _) if terrainTypes.contains(terrain.name) =>
      m.connected = true
    case _ =>
  }
}
