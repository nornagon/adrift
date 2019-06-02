package adrift.items.behaviors

import adrift.items.{Behavior, Item, Message}
import adrift.{GameState, GasComposition, Population}

case class Live(on: Item) extends Message
case class Grow(carbon: Double) extends Message

case class Leaf(
  chanceToDie: Double,
  becomes: Population.Table[String],
  carbonAbsorptionChance: Double,
  carbonAbsorptionChunk: Double,
) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Live(plant) =>
      if (state.random.nextDouble() < chanceToDie) {
        plant.parts = plant.parts.filter(_ ne self)
        for (b <- state.sampleItem(becomes)) {
          state.items.put(b, state.items.lookup(plant))
        }
      } else {
        val pos = state.getItemTile(plant)
        if (state.random.nextDouble() < carbonAbsorptionChance && state.gasComposition(pos).carbonDioxide >= carbonAbsorptionChunk) {
          state.gasComposition(pos) += GasComposition(oxygen = carbonAbsorptionChunk, carbonDioxide = -carbonAbsorptionChunk, nitrogen = 0)
          state.sendMessage(plant, Grow(carbonAbsorptionChunk))
        }
      }
    case _ =>
  }
}

case class Flower(
  lifetime: Double,
  fruit: Population.Table[String],
  var age: Double = 0,
  var fertilized: Boolean = false
) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Live(plant) =>
      if (age <= lifetime) {
        age += 1
      } else {
        plant.parts = plant.parts.filter(_ ne self)
        if (fertilized) {
          for (f <- state.sampleItem(fruit)) {
            state.items.put(f, state.items.lookup(plant))
          }
        }
      }
    case _ =>
  }
}

case class PerennialPlant(
  produces: Population.Table[String],
  productionCarbonThreshold: Double,
  var storedCarbon: Double = 0,
) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Tick =>
      state.broadcastToParts(self, Live(on = self))
      if (storedCarbon >= productionCarbonThreshold) {
        produce(state, self)
        storedCarbon -= productionCarbonThreshold
      }
    case Grow(carbon) =>
      storedCarbon += carbon
    case _ =>
  }

  def produce(state: GameState, self: Item): Unit = {
    val itemKindNames = produces.sample()(state.random, state.data.itemGroups.mapValues(_.choose))
    if (itemKindNames.nonEmpty) {
      val items = itemKindNames.map(itemKindName => state.data.items(itemKindName).generateItem())
      self.parts ++= items
    }
  }
}
