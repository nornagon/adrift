package adrift.items.behaviors

import adrift.{GameState, Inside, Volume}
import adrift.items.Message.{CanContain, CanContainItems, ExtraVolume}
import adrift.items.{Behavior, Item, Message}

case class Container(maxVolume: Volume, rigid: Boolean = false) extends Behavior {
  private def containedVolume(state: GameState, self: Item) =
    state.items.lookup(Inside(self)).view.map(state.volume).foldLeft(Volume(0))(_ + _)
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: CanContainItems => m.ok = true
    case m: CanContain =>
      val vol = state.volume(m.item)
      if (containedVolume(state, self) + vol > maxVolume) {
        m.ok = false
        m.reason = Some("too big")
      }
    case m: ExtraVolume if !rigid =>
      m.volume += containedVolume(state, self)
    case _ =>
  }
}
