package adrift.items

import adrift.GameState
import adrift.items.behaviors._
import adrift.items.behaviors.flora._
import io.circe.{Json, JsonObject}


trait Behavior {
  def tickable: Boolean = false

  def receive(state: GameState, self: Item, message: Message): Unit
}

object Behavior {
  import cats.syntax.functor._
  import io.circe.generic.extras.Configuration
  import io.circe.generic.extras.auto._
  import io.circe.generic.extras.semiauto._
  import io.circe.{Decoder, Encoder}
  implicit private val configuration: Configuration = Configuration.default.withDefaults.withDiscriminator("type")

  val decoders: Map[String, Decoder[Behavior]] = Map(
    "MotionSensor" -> Decoder[MotionSensor].widen,
    "DoorOpener" -> Decoder[DoorOpener].widen,
    "Affixed" -> Decoder[Affixed].widen,
    "Tool" -> Decoder[Tool].widen,
    "HoldsCharge" -> Decoder[HoldsCharge].widen,
    "UsesElectricity" -> Decoder[UsesElectricity].widen,
    "Thermostat" -> Decoder[Thermostat].widen,
    "Heater" -> Decoder[Heater].widen,
    "Wearable" -> Decoder[Wearable].widen,
    "GasLeak" -> Decoder[GasLeak].widen,
    "PerennialPlant" -> Decoder[PerennialPlant].widen,
    "AnnualPlant" -> Decoder[AnnualPlant].widen,
    "Seed" -> Decoder[Seed].widen,
    "Leaf" -> Decoder[Leaf].widen,
    "Flower" -> Decoder[Flower].widen,
    "Pollinator" -> Decoder[Pollinator].widen,
    "Block" -> Decoder[Block].widen,
    "Edible" -> Decoder[Edible].widen,
    "Broken" -> Decoder[Broken].widen,
    "DisplaysConnectedTo" -> Decoder[DisplaysConnectedTo].widen,
    "BumpActivated" -> Decoder[BumpActivated].widen,
    "Diagnosable" -> Decoder[Diagnosable].widen,
    "HasPorts" -> Decoder[HasPorts].widen,
    "AdvancedThermostat" -> Decoder[AdvancedThermostat].widen,
    "ActivatedByDataPort" -> Decoder[ActivatedByDataPort].widen,
    "DebugPrintMessages" -> Decoder[DebugPrintMessages].widen,
    "Container" -> Decoder[Container].widen,
    "ProvidesInfinitePower" -> Decoder[ProvidesInfinitePower].widen,
    "CombustsContents" -> Decoder[CombustsContents].widen,
    "Combustible" -> Decoder[Combustible].widen,
    "Vacuum" -> Decoder[Vacuum].widen,
    "Permeable" -> Decoder[Permeable].widen,
    "Vent" -> Decoder[Vent].widen,
    "AtmoPump" -> Decoder[AtmoPump].widen,
    "AirMix" -> Decoder[AirMix].widen,
    "GasTank" -> Decoder[GasTank].widen,
    "Regulator" -> Decoder[Regulator].widen,
    "InSideRegulator" -> Decoder[InSideRegulator].widen,
    "Compressor" -> Decoder[Compressor].widen,
    "MissingParts" -> Decoder[MissingParts].widen,
  )

  implicit val usesElectricityEncoder: Encoder[UsesElectricity] = deriveConfiguredEncoder

  implicit val encodeBehavior: Encoder[Behavior] = (b: Behavior) => Json.fromJsonObject(JsonObject.singleton(b.getClass.getSimpleName, b match {
    case b: MotionSensor => Encoder[MotionSensor].apply(b)
    case b: Affixed => Encoder[Affixed].apply(b)
    case b: DoorOpener => Encoder[DoorOpener].apply(b)
    case b: Tool => Encoder[Tool].apply(b)
    case b: HoldsCharge => Encoder[HoldsCharge].apply(b)
    case b: UsesElectricity => Encoder[UsesElectricity].apply(b)
    case b: Heater => Encoder[Heater].apply(b)
    case b: Thermostat => Encoder[Thermostat].apply(b)
    case b: Wearable => Encoder[Wearable].apply(b)
    case b: GasLeak => Encoder[GasLeak].apply(b)
    case b: PerennialPlant => Encoder[PerennialPlant].apply(b)
    case b: AnnualPlant => Encoder[AnnualPlant].apply(b)
    case b: Seed => Encoder[Seed].apply(b)
    case b: Leaf => Encoder[Leaf].apply(b)
    case b: Flower => Encoder[Flower].apply(b)
    case b: Pollinator => Encoder[Pollinator].apply(b)
    case b: Block => Encoder[Block].apply(b)
    case b: Edible => Encoder[Edible].apply(b)
    case b: Broken => Encoder[Broken].apply(b)
    case b: DisplaysConnectedTo => Encoder[DisplaysConnectedTo].apply(b)
    case b: BumpActivated => Encoder[BumpActivated].apply(b)
    case b: Diagnosable => Encoder[Diagnosable].apply(b)
    case b: HasPorts => Encoder[HasPorts].apply(b)
    case b: AdvancedThermostat => Encoder[AdvancedThermostat].apply(b)
    case b: ActivatedByDataPort => Encoder[ActivatedByDataPort].apply(b)
    case b: DebugPrintMessages => Encoder[DebugPrintMessages].apply(b)
    case b: Container => Encoder[Container].apply(b)
    case b: ProvidesInfinitePower => Encoder[ProvidesInfinitePower].apply(b)
    case b: CombustsContents => Encoder[CombustsContents].apply(b)
    case b: Combustible => Encoder[Combustible].apply(b)
    case b: Vacuum => Encoder[Vacuum].apply(b)
    case b: Permeable => Encoder[Permeable].apply(b)
    case b: Vent => Encoder[Vent].apply(b)
    case b: AtmoPump => Encoder[AtmoPump].apply(b)
    case b: AirMix => Encoder[AirMix].apply(b)
    case b: GasTank => Encoder[GasTank].apply(b)
    case b: Regulator => Encoder[Regulator].apply(b)
    case b: InSideRegulator => Encoder[InSideRegulator].apply(b)
    case b: Compressor => Encoder[Compressor].apply(b)
    case b: MissingParts => Encoder[MissingParts].apply(b)
  }))
}

