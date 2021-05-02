package adrift.items.behaviors

import adrift.{GameState, OnFloor}
import adrift.items.Message.Tick
import adrift.items.{Behavior, Item, Message}

case class AdvancedThermostat(
  setPoint: Int,
  hysteresis: Int,
  heaterPort: String,
  coolerPort: String
) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Tick =>
      state.items lookup self match {
        case OnFloor(loc) =>
          val t = state.levels(loc.levelId).temperature(loc.xy)
          if (t < setPoint - hysteresis) {
            // send data message on port heaterPort
            state.sendMessage(self, Message.SendDataPacket(heaterPort, 1))
            state.sendMessage(self, Message.SendDataPacket(coolerPort, 0))
          } else if (t > setPoint + hysteresis) {
            state.sendMessage(self, Message.SendDataPacket(heaterPort, 0))
            state.sendMessage(self, Message.SendDataPacket(coolerPort, 1))
            // send data message on port coolerPort
          }
        case _ =>
      }
    case _ =>
  }
}
