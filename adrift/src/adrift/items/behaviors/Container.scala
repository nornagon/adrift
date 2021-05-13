package adrift.items.behaviors

import adrift.{GameState, Inside, OnFloor, Volume}
import adrift.items.Message.{CanContain, CanContainItems, Disassembled, ExtraVolume}
import adrift.items.{Behavior, Item, Message}

case class Container(maxVolume: Volume, rigid: Boolean = false) extends Behavior {
  private def containedVolume(state: GameState, self: Item) =
    state.items.lookup(Inside(self)).view.map(state.volume).foldLeft(Volume(0))(_ + _)
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: CanContainItems => m.ok = state.isFunctional(self)
    case m: CanContain =>
      if (state.isFunctional(self)) {
        val vol = state.volume(m.item)
        if (containedVolume(state, self) + vol > maxVolume) {
          m.ok = false
          m.reason = Some("too big")
        }
      }
    case m: ExtraVolume if !rigid =>
      if (state.isFunctional(self))
        m.volume += containedVolume(state, self)

    case m: Disassembled =>
      val selfLoc = state.getItemTile(self)
      // We are no longer a functioning container, dump contents on the floor
      for (item <- state.items.lookup(Inside(self)))
        state.items.move(item, OnFloor(selfLoc))
    case _ =>
  }
}
