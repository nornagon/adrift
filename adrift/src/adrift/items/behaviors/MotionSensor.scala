package adrift.items.behaviors

import adrift.items.Message.{Activate, Deactivate, PlayerBump, PlayerMove, Tick}
import adrift.{GameState, Location, OnFloor}
import adrift.items._

case class MotionSensor(radius: Int, timer: Int = 6, var activeTicks: Int = 0) extends Behavior {
  override def receive(state: GameState, self: Item, message: Message): Unit = message match {
    case PlayerMove(Location(_, x, y)) =>
      val loc = state.items.lookup(self)
      loc match {
        case OnFloor(Location(_, ix, iy)) =>
          if ((x - ix).abs + (y - iy).abs <= radius) {
            if (activeTicks == 0)
              state.sendMessage(self, Message.SendDataPacket("Active", 1))
            activeTicks = timer
          }
        case _ =>
      }
    case Tick if activeTicks > 0 =>
      activeTicks -= 1
      if (activeTicks == 0) {
        state.sendMessage(self, Message.SendDataPacket("Active", 0))
      }
    case _ =>
  }
}

case class BumpActivated(timer: Int = 6, var activeTicks: Int = 0) extends Behavior {
  override def receive(state: GameState, self: Item, message: Message): Unit = message match {
    case PlayerBump(_) =>
      if (activeTicks == 0)
        state.sendMessage(self, Message.SendDataPacket("Active", 1))
      activeTicks = timer
    case Tick if activeTicks > 0 =>
      activeTicks -= 1
      if (activeTicks == 0)
        state.sendMessage(self, Message.SendDataPacket("Active", 0))
    case _ =>
  }
}
