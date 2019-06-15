package adrift.items.behaviors

import adrift.GasComposition
import adrift.{GameState, OnFloor}
import adrift.items.{Behavior, Item, Message}

case class PumpGas(var gas: GasComposition) extends Message


case class Gasostat(gas: GasComposition, targetPp: Double, hysteresis: Double) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Tick =>
      state.items lookup self match {
        case l @ OnFloor(x, y) =>
          val c = state.gasComposition(x, y)
          if (c.oxygen < targetPp - hysteresis) {
            state.broadcastToLocation(l, Message.Activate)
          } else if (c.oxygen > targetPp) {
            state.broadcastToLocation(l, Message.Deactivate)
          }
        case _ =>
      }
    case _ =>
  }
}

case class GasPump(gas: GasComposition, from: Item, to: Item, var active: Boolean = true) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Activate =>
      active = true
    case Message.Deactivate =>
      active = false
    case Message.Tick if active =>
      val pumpedGas = state.sendMessage(from, PumpGas(gas * -1))
      state.sendMessage(to, PumpGas(gas * -1))
    case _ =>
  }
}

case class GasHolder(var composition: GasComposition, maxPressure: Double) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m@PumpGas(gas) =>
      if (((gas.totalPressure + composition.totalPressure) > maxPressure) || ((gas + composition).minPressure < 0)) {
        m.gas = GasComposition.zero()
      } else {
        composition = composition + gas
      }
    case _ =>
  }
}

case class Diffuser() extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m@PumpGas(gas) =>
      val (x,y) = state.getItemTile(self)
      if ((gas + state.gasComposition(x,y)).minPressure < 0) {
        m.gas = GasComposition.zero()
      } else {
        state.gasComposition(x,y) += gas
      }
    case _ =>
  }

}