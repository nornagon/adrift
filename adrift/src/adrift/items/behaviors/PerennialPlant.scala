package adrift.items.behaviors

import adrift.items.{Behavior, Item, Message}
import adrift.{GameState, Population}

case class PerennialPlant(
  produces: Population.Table[String]
) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Tick =>
      val itemKindNames = produces.sample()(state.random, state.data.itemGroups.mapValues(_.choose))
      if (itemKindNames.nonEmpty) {
        val items = itemKindNames.map(itemKindName => state.data.items(itemKindName).generateItem())
        self.parts ++= items
      }
    case _ =>
  }
}
