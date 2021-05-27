package adrift.items.behaviors

import adrift.items.Message.{AdjustPressureOnPort, GetPressure, ReceivedFluid, Tick, TotalPressure}
import adrift.items.{Behavior, Item, Message}
import adrift.{GameState, GasComposition, OnFloor}

import scala.collection.mutable

case class Vent(portName: String) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: GetPressure if m.port == portName =>
      val loc = state.items.lookup(self)
      loc match {
        case OnFloor(l) =>
          m.totalPressure = Some(state.levels(l.levelId).gasComposition(l.xy))
        case _ =>
          println("WARNING: GetPressure message delivered to item not on floor")
      }
    case m: AdjustPressureOnPort if m.port == portName =>
      val loc = state.items.lookup(self)
      loc match {
        case OnFloor(l) =>
          val gc = state.levels(l.levelId).gasComposition(l.xy)
          state.levels(l.levelId).setGasComposition(l.x, l.y, gc + (m.averagePressure - gc) * m.t)
        case _ =>
          println("WARNING: AdjustPressure message delivered to item not on floor")
      }


    case m @ ReceivedFluid(inPort, mixture) if inPort == portName =>
      if (state.isFunctional(self)) {
        val loc = state.items.lookup(self)
        loc match {
          case OnFloor(l) =>
            val remainder = mutable.Map.empty[String, Float]
            var gc = state.levels(l.levelId).gasComposition(l.xy)
            for ((k, v) <- mixture) {
              k match {
                case "oxygen" =>
                  gc += GasComposition(v, 0, 0)
                case "carbon dioxide" =>
                  gc += GasComposition(0, v, 0)
                case "nitrogen" =>
                  gc += GasComposition(0, 0, v)
                case _ =>
                  remainder += (k -> v)
              }
            }
            m.mixture = remainder.toMap
            state.levels(l.levelId).setGasComposition(l.x, l.y, gc)
          case _ =>
        }
      }
    case _ =>
  }
}

case class AtmoPump(portName: String) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Tick =>
      if (state.isFunctional(self)) {
        val loc = state.items.lookup(self)
        loc match {
          case OnFloor(l) =>
            val gc = state.levels(l.levelId).gasComposition(l.xy)
            val mixture = Map(
              "oxygen" -> gc.oxygen,
              "nitrogen" -> gc.nitrogen,
              "carbon dioxide" -> gc.carbonDioxide,
            )
            val remainder = state.sendMessage(self, Message.PushFluid(portName, mixture)).mixture

            state.levels(l.levelId).setGasComposition(l.x, l.y, GasComposition(
              remainder.getOrElse("oxygen", 0),
              remainder.getOrElse("carbon dioxide", 0),
              remainder.getOrElse("nitrogen", 0),
            ))
        }
      }
    case _ =>
  }
}
