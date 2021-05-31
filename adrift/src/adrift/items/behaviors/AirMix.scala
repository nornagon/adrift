package adrift.items.behaviors

import adrift.{GameState, GasComposition}
import adrift.items.Message.{AdjustPressureOnPort, GetPressure}
import adrift.items.{Behavior, Item, Message}

case class AirMix(
  o2Port: String,
  co2Port: String,
  n2Port: String,
  atmoInPort: String,
  atmoOutPort: String,
  var internalPressure: GasComposition = GasComposition.zero,
) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: GetPressure if m.port == atmoInPort || m.port == atmoOutPort =>
      m.totalPressure = Some(internalPressure)

    case m: AdjustPressureOnPort if m.port == atmoInPort || m.port == atmoOutPort =>
      internalPressure += (m.averagePressure - internalPressure) * m.t

    case m: GetPressure if m.port == o2Port =>
      m.totalPressure = Some(GasComposition(oxygen = internalPressure.oxygen, 0, 0))

    case m: AdjustPressureOnPort if m.port == o2Port =>
      internalPressure += (m.averagePressure - GasComposition(oxygen = internalPressure.oxygen, 0, 0)) * m.t

    case m: GetPressure if m.port == co2Port =>
      m.totalPressure = Some(GasComposition(0, carbonDioxide = internalPressure.carbonDioxide, 0))

    case m: AdjustPressureOnPort if m.port == co2Port =>
      internalPressure += (m.averagePressure - GasComposition(0, carbonDioxide = internalPressure.carbonDioxide, 0)) * m.t

    case m: GetPressure if m.port == n2Port =>
      m.totalPressure = Some(GasComposition(0, 0, nitrogen = internalPressure.nitrogen))

    case m: AdjustPressureOnPort if m.port == n2Port =>
      internalPressure += (m.averagePressure - GasComposition(0, 0, nitrogen = internalPressure.nitrogen)) * m.t

    case _ =>
  }
}
