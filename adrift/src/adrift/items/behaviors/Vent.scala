package adrift.items.behaviors

import adrift.items.Message.{AdjustPressureOnPort, GetPressure, Tick}
import adrift.items.{Behavior, Item, Message}
import adrift.{GameState, GasComposition, OnFloor}

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

    case _ =>
  }
}

case class AtmoPump(
  inPort: String,
  outPort: String,
  maxPressureGradient: Float,
  var internalPressureIn: GasComposition = GasComposition.zero,
  var internalPressureOut: GasComposition = GasComposition.zero,
) extends Behavior {
  // The atmo pump has 2 tanks. One is equalized with the in port, one is equalized with the out port.
  // Each tick, the pump moves half the gas from the in tank to the out tank.
  // If the pressure gradient exceeds |maxPressureGradient|, the pump stops working.
  private val internalTankVolume = 100f

  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: GetPressure =>
      if (m.port == inPort) {
        m.totalPressure = Some((internalPressureIn, internalTankVolume))
      } else if (m.port == outPort) {
        m.totalPressure = Some((internalPressureOut, internalTankVolume))
      }
    case m: AdjustPressureOnPort =>
      if (m.port == inPort) {
        val delta = (m.averagePressure - internalPressureIn) * m.t
        internalPressureIn += delta
      } else if (m.port == outPort) {
        val delta = (m.averagePressure - internalPressureOut) * m.t
        internalPressureOut += delta
      }

    case Tick =>
      val diff = internalPressureOut.totalPressure - internalPressureIn.totalPressure
      if (diff < maxPressureGradient) { // if the gradient is shallow enough, we can pump against it
        val delta = internalPressureIn * 0.5f
        internalPressureIn -= delta
        internalPressureOut += delta
      }
    case _ =>
  }
}

/*
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
 */

case class GasTank(
  port: String,
  var internalPressure: GasComposition,
) extends Behavior {
  private val internalVolume = 100f

  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: GetPressure =>
      m.totalPressure = Some((internalPressure, internalVolume))

    case m: AdjustPressureOnPort if m.port == port =>
      val delta = (m.averagePressure - internalPressure) * m.t
      internalPressure += delta

    case _ =>
  }
}

case class Regulator(
  inPort: String,
  outPort: String,
  var targetPressure: Float = 0f,
  var inSidePressure: GasComposition = GasComposition.zero,
  var outSidePressure: GasComposition = GasComposition.zero
) extends Behavior {
  val internalVolume = 100f
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: GetPressure if m.port == inPort =>
      m.totalPressure = Some((inSidePressure, internalVolume))
    case m: AdjustPressureOnPort if m.port == inPort =>
      val delta = (m.averagePressure - inSidePressure) * m.t
      inSidePressure += delta

    case m: GetPressure if m.port == outPort =>
      m.totalPressure = Some((outSidePressure, internalVolume))
    case m: AdjustPressureOnPort if m.port == outPort =>
      val delta = (m.averagePressure - outSidePressure) * m.t
      outSidePressure += delta

    case Tick =>
      if (outSidePressure.totalPressure < targetPressure && inSidePressure.totalPressure > outSidePressure.totalPressure) {
        val deltaSize = (math.min(targetPressure, inSidePressure.totalPressure) - outSidePressure.totalPressure) * 0.5f
        val delta = inSidePressure / inSidePressure.totalPressure * deltaSize
        inSidePressure -= delta
        outSidePressure += delta
      }
    case _ =>
  }
}

case class Compressor(
  inPort: String,
  outPort: String,
  maxPressure: Float,
  var inSidePressure: GasComposition = GasComposition.zero,
  var outSidePressure: GasComposition = GasComposition.zero,
) extends Behavior {
  val inSideVolume = 100f
  val outSideVolume = 10f
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: GetPressure if m.port == inPort =>
      m.totalPressure = Some((inSidePressure, inSideVolume))
    case m: AdjustPressureOnPort if m.port == inPort =>
      val delta = (m.averagePressure - inSidePressure) * m.t
      inSidePressure += delta

    case m: GetPressure if m.port == outPort =>
      m.totalPressure = Some((outSidePressure, outSideVolume))
    case m: AdjustPressureOnPort if m.port == outPort =>
      val delta = (m.averagePressure - outSidePressure) * m.t
      outSidePressure += delta

    case Tick =>
      val movedAmountOfSubstance = inSidePressure * inSideVolume * 0.1f
      inSidePressure -= movedAmountOfSubstance / inSideVolume
      outSidePressure += movedAmountOfSubstance / outSideVolume
      // TODO: handle over-pressure.

    case _ =>
  }
}
