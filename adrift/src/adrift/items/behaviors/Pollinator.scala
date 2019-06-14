package adrift.items.behaviors

import adrift.{GameState, OnFloor}
import adrift.items.{Behavior, Item, Message}
import adrift.RandomImplicits._

case class Pollinator(
  var bState: String = "landed",
  var energy: Double = 0,
) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Tick =>
      bState match {
        case "landed" =>
          if (energy >= 5 && state.random.nextDouble() < 0.01) {
            bState = "flying"
          }
          energy += 0.1
        case "flying" if energy <= 1 =>
          bState = "landed"
          state.broadcastToLocation(state.items.lookup(self), Pollinate())
        case "flying" =>
          val (dx, dy) = state.random.oneOf((0, 0), (-1, 0), (1, 0), (0, -1), (0, 1))
          val (x, y) = state.getItemTile(self)
          if ((dx != 0 || dy != 0) && state.canWalk(x + dx, y + dy)) {
            state.items.move(self, OnFloor(x + dx, y + dy))
            energy -= 1
          } else {
            energy -= 0.25
          }
      }
    case m: Message.Display if bState == "landed" =>
      m.display = "INVISIBLE"
    case _ =>
  }
}
