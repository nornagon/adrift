package adrift.items.behaviors.flora

import adrift.items.{Behavior, Item, Message}
import adrift.{GameState, GasComposition, Population}

case class MycelialMat(
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

case class Mushroom(
  lifetime: Double,
  fruit: Population.Table[String],
  var age: Double = 0,
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
        plant.parts ++= state.sampleItem(fruit)
      }
    case _ =>
  }
}
