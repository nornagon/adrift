package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

case class DebugPrintMessages(filter: Option[String], trace: Boolean = false) extends Behavior {
  private val filterRe = filter.map(_.r.unanchored)
  println(filterRe)
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = {
    if (filterRe.isEmpty || filterRe.get.matches(message.getClass.getSimpleName)) {
      println(s"${self.kind.name}(${self.id.id}): $message")
      if (trace)
        (new Throwable).printStackTrace()
    }
  }
}
