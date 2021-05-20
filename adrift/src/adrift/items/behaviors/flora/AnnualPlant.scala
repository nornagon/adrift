package adrift.items.behaviors.flora

import adrift.items.{Behavior, Item, Message}
import adrift.{GameState, GasComposition, Population}



case class AnnualPlant(
  produces: Population.Table[String],
  productionCarbonThreshold: Double,
  minLifespanSeconds: Int,
  maxLifespanSeconds: Int,
  becomes: Population.Table[String],
  var storedCarbon: Double = 0,
  var livedSeconds: Int = 0
) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Tick =>
      state.broadcastToParts(self, Live(on = self, light = 1))
      if (storedCarbon >= productionCarbonThreshold) {
        produce(state, self)
        storedCarbon -= productionCarbonThreshold
      }
      livedSeconds += 1
      if (livedSeconds > minLifespanSeconds) {
        val deathchance = (minLifespanSeconds - livedSeconds).toDouble / (maxLifespanSeconds - minLifespanSeconds).toDouble
        if (state.random.nextDouble() < deathchance) {
          state.broadcastToParts(self, Die(self))
        }
      }
    case Grow(carbon) =>
      storedCarbon += carbon
    case Pollinate() =>
      state.broadcastToParts(self, message)
    case Die(plant) =>
      for (b <- state.sampleItemOnly(becomes)) {
        state.items.put(b, state.items.lookup(plant))
      }
      state.items.delete(plant)
    case _ =>
  }

  def produce(state: GameState, self: Item): Unit =
    self.parts ++= state.sampleItemOnly(produces)
}
