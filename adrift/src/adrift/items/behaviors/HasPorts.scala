package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

case class PortSpec(
  `type`: String,
  name: String
)

case class HasPorts(ports: Seq[PortSpec], var connections: Map[String, Int] = Map.empty) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case _ =>
  }
}
