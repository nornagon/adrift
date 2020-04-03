package adrift.items.behaviors

import adrift.items.Message.CanReceivePlug
import adrift.{GameState, Location, OnFloor}
import adrift.items.{Behavior, Item, Message}

case class Cable(plugShape: String) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: Message.CanPlugInto =>
      if (state.sendMessage(m.item, CanReceivePlug(plugShape)).ok)
        m.ok = true
    case Message.PlugInto(item) =>
      if (state.sendMessage(item, Message.CanReceivePlug(plugShape)).ok) {
        if (!self.behaviors.exists(_.isInstanceOf[Unspooling])) {
          // first plug action begins the unspooling
          self.behaviors.append(Unspooling())
          // unspool from the location of the plugee to the player's position
          val OnFloor(loc) = state.items.lookup(item)
          if (loc != state.player)
            state.sendMessage(self, Message.Hauled(loc, state.player))
        } else {
          // if we're already unspooling, this must be the 2nd plug action
          self.behaviors --= self.behaviors.filter(_.isInstanceOf[Unspooling])
          state.items.delete(self)
          val OnFloor(loc) = state.items.lookup(item)
          if (loc != state.player)
            state.sendMessage(self, Message.Hauled(loc, state.player))
          if (!self.parts.exists(state.items.exists)) {
            // no part of the cable is on the floor, so drop one to make it pick-up-able
            state.items.put(self.parts.head, state.items.lookup(item))
          }
        }
        item.behaviors.append(Plugged(self))
        self.behaviors.append(Plugged(item))
      }
    case Message.Unplugged() =>
      if (!state.items.exists(self)) {
        val location = self.parts
          .collectFirst { case i if state.items.exists(i) => state.items.lookup(i) }
          .getOrElse(OnFloor(state.player))
        self.parts.foreach { p =>
          if (state.items.exists(p)) state.items.delete(p)
          p.behaviors --= p.behaviors.filter(_.isInstanceOf[Unrolled])
        }
        state.items.put(self, location)
      }
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
      state.sendMessage(parent, Message.Unplugged())
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
      var lastCell = from.xy
      for (c <- cellsBetween(from.xy, to.xy)) {
        val section = self.parts.find(!_.behaviors.exists(_.isInstanceOf[Unrolled]))
        val prevSection = self.parts.filter(_.behaviors.exists(_.isInstanceOf[Unrolled])).lastOption
        section foreach { s =>
          state.items.put(s, OnFloor(Location(from.levelId, c._1, c._2)))
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
        self.behaviors -= this
      }
    case Message.Dropped() =>
      self.behaviors -= this
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

/** a message sent from one end of a plug to another */
case class PlugMessage(wrapped: Message, from: Item) extends Message

case class Plugged(other: Item) extends Behavior {
  private var receiving = false // not serialized because it's only used during dispatch
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: Message.ChargeAvailable if !receiving =>
      state.sendMessage(other, PlugMessage(m, self))
    case m: Message.DrawCharge if !receiving =>
      state.sendMessage(other, PlugMessage(m, self))
    case PlugMessage(m, _) =>
      // if we just received a ChargeAvailable message from the other side of the plug, we don't want to send it back
      // to that side, so temporarily turn off our message transmission functionality.
      receiving = true
      state.sendMessage(self, m)
      receiving = false
    case _: Message.PickedUp =>
      state.sendMessage(self, Message.Unplugged())
    case m: Message.Unplugged =>
      self.behaviors -= this
      state.sendMessage(other, PlugMessage(m, self))
    case _ =>
  }
}

case class Socket(shape: String) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m: CanReceivePlug if m.plugShape == shape =>
      m.ok = true
    case _ =>
  }
}
