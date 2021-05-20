package adrift.items.behaviors.flora


import adrift.YamlObject.ItemWithExtras
import adrift.items.{Behavior, Item, Message}
import adrift.{GameState, GasComposition, Population}

case class Die(from: Item) extends Message
case class Live(on: Item, var light: Double) extends Message
case class Grow(carbon: Double) extends Message
case class Pollinate() extends Message

case class Leaf(
  chanceToDie: Double,
  becomes: Population.Table[String],
  carbonAbsorptionChance: Float,
  carbonAbsorptionChunk: Float,
) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case msg @ Die(plant) => {
      plant.parts = plant.parts.filter(_ ne self)
      for (b <- state.sampleItem(becomes.map(ItemWithExtras(_)))) {
        state.items.put(b, state.items.lookup(plant))
      }
    }
    case msg @ Live(plant, _) =>
      if (state.random.nextDouble() < chanceToDie) {
        plant.parts = plant.parts.filter(_ ne self)
        for (b <- state.sampleItem(becomes.map(ItemWithExtras(_)))) {
          state.items.put(b, state.items.lookup(plant))
        }
      } else {
        if (state.random.nextDouble() < msg.light) {
          val pos = state.getItemTile(plant)
          val level = state.levels(pos.levelId)
          if (state.random.nextDouble() < carbonAbsorptionChance &&
            level.gasComposition(pos.xy).carbonDioxide >= carbonAbsorptionChunk) {
            val gc = level.gasComposition(pos.xy)
            level.setGasComposition(pos.x, pos.y, gc + GasComposition(oxygen = carbonAbsorptionChunk, carbonDioxide = -carbonAbsorptionChunk, nitrogen = 0))
            state.sendMessage(plant, Grow(carbonAbsorptionChunk))
          }
        }
        msg.light *= 0.95
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
    case Live(plant, _) =>
      if (age <= lifetime) {
        age += 1
      } else {
        plant.parts = plant.parts.filter(_ ne self)
        if (fertilized) {
          plant.parts ++= state.sampleItem(fruit.map(ItemWithExtras(_)))
        }
      }
    case Pollinate() =>
      fertilized = true
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
      state.broadcastToParts(self, Live(on = self, light = 1))
      if (storedCarbon >= productionCarbonThreshold) {
        produce(state, self)
        storedCarbon -= productionCarbonThreshold
      }
    case Grow(carbon) =>
      storedCarbon += carbon
    case Pollinate() =>
      state.broadcastToParts(self, message)
    case _ =>
  }

  def produce(state: GameState, self: Item): Unit =
    self.parts ++= state.sampleItem(produces.map(ItemWithExtras(_)))
}
