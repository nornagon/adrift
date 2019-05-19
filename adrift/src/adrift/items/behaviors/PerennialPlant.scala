package adrift.items.behaviors

import adrift.items.{Behavior, Item, Message}
import adrift.{GameState, GasComposition, Population}

case class PerennialPlant(
  produces: Population.Table[String],
  carbonAbsorptionChance: Double,
  carbonAbsorptionChunk: Double,
  productionCarbonThreshold: Double,
  var storedCarbon: Double = 0,
) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Tick =>
      val pos = state.getItemTile(self)
      if (state.random.nextDouble() < carbonAbsorptionChance && state.gasComposition(pos).carbonDioxide >= carbonAbsorptionChunk) {
        state.gasComposition(pos) += GasComposition(oxygen = carbonAbsorptionChunk, carbonDioxide = -carbonAbsorptionChunk, nitrogen = 0)
        storedCarbon += carbonAbsorptionChunk
        if (storedCarbon >= productionCarbonThreshold) {
          produce(state, self)
          storedCarbon -= productionCarbonThreshold
        }
      }
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
