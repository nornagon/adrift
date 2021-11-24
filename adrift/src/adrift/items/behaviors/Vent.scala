package adrift.items.behaviors

import adrift.items.Message.{AdjustPressureOnPort, GetPressure, ReceivedFluid}
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
          m.totalPressure = Some((state.levels(l.levelId).gasComposition(l.xy), 1000))
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

case class AtmoPump(
  inPort: String,
  outPort: String,
  minPressure: Float,
  var internalPressure: GasComposition = GasComposition.zero,
  var blocked: Boolean = false
) extends Behavior {
  private val internalVolume = 100f

  private def inPortPressure =
    if (internalPressure.totalPressure < minPressure) {
      internalPressure
    } else {
      internalPressure * (minPressure / internalPressure.totalPressure)
    }

  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {

    case m: GetPressure if m.port == inPort && !blocked =>
      m.totalPressure = Some((inPortPressure, internalVolume))

    case m: AdjustPressureOnPort if m.port == inPort =>
      if (!blocked) {
        val delta = (m.averagePressure - inPortPressure) * m.t
        internalPressure += delta
      }
      // We block the valve if the pressure on the network falls below our min.
      blocked = m.averagePressure.totalPressure < minPressure

    case m: GetPressure if m.port == outPort =>
      m.totalPressure = Some((internalPressure, internalVolume))

    case m: AdjustPressureOnPort if m.port == outPort =>
      val delta = (m.averagePressure - internalPressure) * m.t
      internalPressure += delta


      /*
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
       */
    case _ =>
  }
}

case class GasTank(
  port: String,
  var internalPressure: GasComposition,
  var regulatedPressure: Float,
) extends Behavior {
  private val internalVolume = 100f

  private def regulatedPressureComposition =
    if (internalPressure.totalPressure < regulatedPressure) {
      internalPressure
    } else {
      internalPressure * (regulatedPressure / internalPressure.totalPressure)
    }

  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: GetPressure =>
      m.totalPressure = Some((regulatedPressureComposition, internalVolume))

    case m: AdjustPressureOnPort if m.port == port =>
      val delta = (m.averagePressure - regulatedPressureComposition) * m.t
      internalPressure += delta

    case _ =>
  }
}
