package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

case class Seed(
  growsInto: String,
  germination: Seed.Germination,
  survival: Seed.Survival,
  var dead: Boolean = false,
  var germinationTimer: Double = 0,
) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Tick if !dead =>
      val pos = state.getItemTile(self)
      val gas = state.gasComposition(pos)
      val temp = state.temperature(pos)
      if (gas.totalPressure() < survival.minPressure || temp < survival.minTemperature || temp > survival.maxTemperature) {
        dead = true
      }
      if (!dead) {
        if (gas.totalPressure() >= germination.minPressure && temp >= germination.minTemperature && temp <= germination.maxTemperature) {
          germinationTimer += 1
        } else {
          germinationTimer = 0
        }
        if (germinationTimer >= germination.duration) {
          val loc = state.items.lookup(self)
          val item = state.data.items(growsInto).generateItem()
          state.items.delete(self)
          state.items.put(item, loc)
        }
      }
    case _ =>
  }
}

object Seed {
  case class Germination(
    minTemperature: Double,
    maxTemperature: Double,
    minPressure: Double,
    duration: Double,
  )
  case class Survival(
    minTemperature: Double,
    maxTemperature: Double,
    minPressure: Double,
  )
}
