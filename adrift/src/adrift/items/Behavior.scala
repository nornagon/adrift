package adrift.items

import adrift.GameState
import adrift.items.behaviors._
import io.circe.{Json, JsonObject}

trait Behavior {
  def receive(state: GameState, self: Item, message: Message): Unit
}

object Behavior {
  import cats.syntax.functor._
  import io.circe.{Decoder, Encoder}
  import io.circe.generic.extras.Configuration
  import io.circe.generic.extras.auto._
  import io.circe.generic.extras.semiauto._
  implicit private val configuration: Configuration = Configuration.default.withDefaults.withDiscriminator("type")

  val decoders: Map[String, Decoder[Behavior]] = Map(
    "MotionSensor" -> Decoder[MotionSensor].widen,
    "DoorOpener" -> Decoder[DoorOpener].widen,
    "Affixed" -> Decoder[Affixed].widen,
    "Tool" -> Decoder[Tool].widen,
    "HoldsCharge" -> Decoder[HoldsCharge].widen,
    "UsesElectricity" -> Decoder[UsesElectricity].widen,
    "Cable" -> Decoder[Cable].widen,
    "ProvidesShipPower" -> Decoder[ProvidesShipPower].widen,
    "GeneratesShipPower" -> Decoder[GeneratesShipPower].widen,
    "Socket" -> Decoder[Socket].widen,
    "Thermostat" -> Decoder[Thermostat].widen,
    "Heater" -> Decoder[Heater].widen,
    "Wearable" -> Decoder[Wearable].widen,
    "GasLeak" -> Decoder[GasLeak].widen,
    "PerennialPlant" -> Decoder[PerennialPlant].widen,
    "Seed" -> Decoder[Seed].widen,
    "Leaf" -> Decoder[Leaf].widen,
  )

  implicit val usesElectricityEncoder: Encoder[UsesElectricity] = deriveEncoder

  implicit val encodeBehavior: Encoder[Behavior] = (b: Behavior) => Json.fromJsonObject(JsonObject.singleton(b.getClass.getSimpleName, b match {
    case b: MotionSensor => Encoder[MotionSensor].apply(b)
    case b: Affixed => Encoder[Affixed].apply(b)
    case b: DoorOpener => Encoder[DoorOpener].apply(b)
    case b: Tool => Encoder[Tool].apply(b)
    case b: HoldsCharge => Encoder[HoldsCharge].apply(b)
    case b: UsesElectricity => Encoder[UsesElectricity].apply(b)
    case b: Heater => Encoder[Heater].apply(b)
    case b: Cable => Encoder[Cable].apply(b)
    case b: Thermostat => Encoder[Thermostat].apply(b)
    case b: Wearable => Encoder[Wearable].apply(b)
    case b: GasLeak => Encoder[GasLeak].apply(b)
    case b: PerennialPlant => Encoder[PerennialPlant].apply(b)
    case b: Seed => Encoder[Seed].apply(b)
    case b: Leaf => Encoder[Leaf].apply(b)
  }))
}

