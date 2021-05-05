package adrift.items.behaviors

import adrift.{GameState, Inside, Volume}
import adrift.items.Message.{CanContainItems, ExtraVolume}
import adrift.items.{Behavior, Item, Message}

case class Container(maxVolume: Volume, rigid: Boolean = false) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: CanContainItems => m.ok = true
    case m: ExtraVolume if !rigid =>
      val containedVolume = state.items.lookup(Inside(self)).view.map(state.volume).reduce(_ + _)
      m.volume += containedVolume
    case _ =>
  }
}
