package adrift.items

import adrift.GameState
import adrift.items.behaviors._

trait Behavior {
  def receive(state: GameState, self: Item, message: Message): Unit
}

object Behavior {
  import cats.syntax.functor._
  import io.circe.Decoder
  import io.circe.generic.extras.Configuration
  import io.circe.generic.extras.auto._
  implicit private val configuration: Configuration = Configuration.default.withDefaults

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
  )
}

