package adrift.items

import adrift.items.behaviors.LayerSet
import adrift.{GasComposition, Location, Terrain, Volume}

trait Message

object Message {
  case class PlayerMove(loc: Location) extends Message
  case class PlayerBump(loc: Location) extends Message
  case object Activate extends Message
  case object Deactivate extends Message
  case object Tick extends Message
  case class IsOpaque(var opaque: Boolean = false) extends Message
  case class IsWalkable(var walkable: Boolean = true) extends Message
  case class IsPermeable(var permeable: Boolean = true) extends Message
  case class CanPickUp(var ok: Boolean = true) extends Message
  case class PickUp(var ok: Boolean = true) extends Message
  case class Display(var display: String) extends Message
  case class DisplayConnectedTo(terrain: Terrain, var connected: Boolean = false) extends Message

  case class Conditions(var conditions: Seq[String] = Seq.empty) extends Message
  case class VisibleConditions(var conditions: Seq[String]) extends Message

  case class IsFunctional(var functional: Boolean = true) extends Message

  case class DescriptiveTraits(var traits: Seq[String] = Seq.empty) extends Message

  case class Disassembled() extends Message

  // Tools
  case class UseTool(op: ItemOperation, var ok: Boolean = false) extends Message
  case class ToolUsed(op: ItemOperation) extends Message
  case class Provides(op: ItemOperation, var provides: Boolean = false) extends Message

  // Power
  case class AddCharge(var amount: Int) extends Message
  case class DrawCharge(amount: Int, var ok: Boolean = true) extends Message
  case class ChargeAvailable(amount: Int, var ok: Boolean = false) extends Message
  case class IsCombustible(var ok: Boolean = false) extends Message
  case class CombustionEnergy(var joules: Int = 0) extends Message

  // Moving items
  case class Hauled(from: Location, to: Location) extends Message
  case class PickedUp(var item: Item) extends Message
  case class Dropped() extends Message

  // Clothing
  case class CanWear(var ok: Boolean = false) extends Message
  case class LoseHeat(var dq: Float) extends Message

  // Food
  case class Eat(var calories: Int = 0) extends Message
  case class IsEdible(var edible: Boolean = false) extends Message

  // Diagnosis
  case class Diagnose() extends Message
  case class IsDiagnosable(var diagnosable: Boolean = false, var opRequired: Option[ItemOperation] = None) extends Message
  case class IsDiagnosed(var diagnosed: Boolean = false) extends Message

  // Cables
  case class IsConnected(cableType: String, layers: LayerSet, var connected: Boolean = false) extends Message
  case class SendDataPacket(outPort: String, value: Byte) extends Message
  case class DataPacket(layers: LayerSet, value: Byte) extends Message
  case class ReceivedDataPacket(inPort: String, value: Byte) extends Message

  case class TotalPressure(layer: Int, var totalAmountOfSubstance: GasComposition, var totalVolume: Float) extends Message
  case class AdjustPressure(layer: Int, averagePressure: GasComposition, t: Float) extends Message
  case class GetPressure(port: String, var totalPressure: Option[(GasComposition, Float)]) extends Message
  case class AdjustPressureOnPort(port: String, averagePressure: GasComposition, t: Float) extends Message

  // Containers
  case class CanContainItems(var ok: Boolean = false) extends Message
  case class CanContain(item: Item, var ok: Boolean = true, var reason: Option[String] = None) extends Message
  case class ExtraVolume(var volume: Volume = Volume(0)) extends Message
}
