package adrift.items.behaviors

import adrift.{GameState, OnFloor}
import adrift.items.{Behavior, Item, Message}

case class Cable() extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.PlugInto(item) =>
      self.behaviors.append(Unspooling())
      // also: connect to the item somehow?
    case _ =>
  }
}

case class Unrolled(parent: Item, fromCell: (Int, Int), var toCell: Option[(Int, Int)] = None) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: Message.PickedUp =>
      m.item = parent
      if (state.items.exists(parent)) state.items.delete(parent)
      state.items.put(m.item, state.items.lookup(self))
      parent.parts.foreach { p =>
        if (state.items.exists(p)) state.items.delete(p)
        p.behaviors --= p.behaviors.filter(_.isInstanceOf[Unrolled])
      }
      parent.behaviors --= parent.behaviors.filter(_.isInstanceOf[Unspooling])
    case _ =>
  }
}

case class Unspooling() extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Message.Hauled(from, to) =>
      var lastCell = from
      for (c <- cellsBetween(from, to)) {
        val section = self.parts.find(!_.behaviors.exists(_.isInstanceOf[Unrolled]))
        val prevSection = self.parts.filter(_.behaviors.exists(_.isInstanceOf[Unrolled])).lastOption
        section foreach { s =>
          state.items.put(s, OnFloor(c._1, c._2))
          s.behaviors.append(Unrolled(self, lastCell))
          prevSection foreach { p =>
            p.behaviors.collectFirst { case b: Unrolled => b }.get.toCell = Some(c)
          }
        }
        lastCell = c
      }
      val remaining = self.parts.filter(!_.behaviors.exists(_.isInstanceOf[Unrolled]))
      if (remaining.isEmpty) {
        state.items.delete(self) // it'll come back when a section is picked up
        self.behaviors.remove(self.behaviors.indexOf(this))
      }
    case Message.Dropped() =>
      self.behaviors.remove(self.behaviors.indexOf(this))
      if (self.parts.exists(_.behaviors.exists(_.isInstanceOf[Unrolled])))
        state.items.delete(self)
    case _ =>
  }

  def cellsBetween(from: (Int, Int), to: (Int, Int)): Seq[(Int, Int)] = {
    assert(from._1 - to._1 <= 1)
    assert(from._2 - to._2 <= 1)
    (to._1 - from._1, to._2 - from._2) match {
      case (-1, 0) | (1, 0) | (0, -1) | (0, 1) => Seq(to)
      case _ => Seq((from._1, to._2), to)
    }
  }
}
