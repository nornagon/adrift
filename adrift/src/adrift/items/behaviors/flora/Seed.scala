package adrift.items.behaviors.flora

import adrift.{GameState, Location, OnFloor}
import adrift.items.{Behavior, Item, Message}
import adrift.RandomImplicits._

case class Seed(
  growsInto: String,
  germination: Seed.Germination,
  survival: Seed.Survival,
  initialCarbon: Double,
  var dead: Boolean = false,
  var germinationTimer: Double = 0,
  var driftDist: Int = 1
) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Tick if !dead =>
      val pos = state.getItemTile(self)
      val gas = state.levels(pos.levelId).gasComposition(pos.xy)
      val temp = state.levels(pos.levelId).temperature(pos.xy)
      if (gas.totalPressure < survival.minPressure || temp < survival.minTemperature || temp > survival.maxTemperature) {
        dead = true
      }
      if (!dead) {
        if (gas.totalPressure >= germination.minPressure && temp >= germination.minTemperature && temp <= germination.maxTemperature) {
          germinationTimer += 1
        } else {
          germinationTimer = 0
        }
        if (germinationTimer >= germination.duration) {
          val loc = state.items.lookup(self)
          val item = state.data.items(growsInto).generateItem()
          state.sendMessage(item, Grow(carbon = 5))
          state.items.delete(self)
          state.items.put(item, loc)
        }
      }
    case Live(plant, _) =>
      if (state.random.nextDouble() < 0.1) {
        def drift(initpos: Location, distance: Int): Location =
          if (distance == 0) initpos
          else {
            val (dx, dy) = state.random.oneOf((0, 0), (-1, 0), (1, 0), (0, -1), (0, 1))
            if (state.canWalk(initpos + (dx, dy))) drift(initpos + (dx, dy), distance - 1)
            else initpos
          }
        val pos = drift(state.getItemTile(plant), driftDist)
        plant.parts = plant.parts.filter(_ ne self)
        state.items.put(self, OnFloor(pos))
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
