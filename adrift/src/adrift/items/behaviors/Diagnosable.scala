package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

case class Diagnosable(op: String, var diagnosed: Boolean = false) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: Message.VisibleConditions =>
      if (!diagnosed)
        m.conditions = Seq.empty

    case _: Message.Diagnose =>
      diagnosed = true

    case m: Message.IsDiagnosable =>
      m.diagnosable = !diagnosed //true

    case m: Message.IsDiagnosed =>
      m.diagnosed = diagnosed

    case _ =>
  }
}
