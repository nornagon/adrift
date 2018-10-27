package adrift.items

import adrift.{GameState, OnFloor}

import scala.collection.mutable

/*
Items have some generic type (a battery, a screwdriver, a tomato) and some specific state (half charged, found in the
elevator, moldy).

Items are made out of other specific items, which in turn have their own generic type and specific state.

The kinds of things in an item's specific state differ based on the type. For example, it doesn't make sense for a
tomato to be half-charged, or for a battery to be moldy (unless it was some kind of bio-battery...).

Some items have one or more "functions", i.e. things you can do with them. They could be either direct functions (turn
on the heater, listen to the recording, saw the log in half) or indirect functions (use the plasma torch in crafting,
wear the haz-mat suit, provide light).

For items with a function, all the parts of that item must recursively be functional for the item itself to be
functional. If an item is comprised of other items, it is functional iff all its components are functional. An item
can't be made out of functional things and yet itself be non-functional.

Items can be damaged in a variety of ways. Some damage is trivially easy to repair (e.g. recharge a battery), while
other kinds of damage require specialty tools just to diagnose, let alone repair.

=> so not only do items have specific statuses, the player might also have a varying amount of information about an
item's status. Do they know that the turbolift isn't working because one of the power relays has a broken fuse? Or can
they just tell that the power supply seems to be out of order? (Or just that the buttons don't seem to do anything?)


*****

Where can items be?
- on the ground
- in your hands
- in a container on the ground
- worn on your body
- in a container worn on your body
- inside your body (implants)
- part of another item

What can you do with items?
Simple:
- pick up
- put down
- store in container
Less simple:
- eat
- drink
- disassemble
- assemble
- wear

 */

case class ItemKind(
  name: String,
  description: String,
  parts: Seq[((ItemKind, Int), ItemOperation)],
  provides: Seq[ItemOperation],
  display: String,
  behaviors: Seq[() => Behavior]
)

trait ItemCondition {
  def functional: Boolean = false
}

class ItemId(val id: Int) extends AnyVal
object ItemId {
  private var _nextId = 1
  def next: ItemId = { _nextId += 1; new ItemId(_nextId) }
}

case class Item(
  kind: ItemKind,
  conditions: mutable.Buffer[ItemCondition],
  parts: Seq[Item],
  behaviors: mutable.Buffer[Behavior]
) {
  def functional: Boolean = conditions.forall(_.functional) && parts.forall(_.functional)
  val id: ItemId = ItemId.next
  override def hashCode(): Int = id.id
  override def equals(obj: Any): Boolean = obj.isInstanceOf[Item] && obj.asInstanceOf[Item].id == id

  override def toString: String = s"Item(id=$id, kind=${kind.name})"
}


case class Charge(kwh: Double, maxKwh: Double) extends ItemCondition {
  override def functional: Boolean = kwh > 0
}

case object Broken extends ItemCondition {
  override def functional: Boolean = false
}

/** Some action that you can do with an item, e.g. PRYING or WELDING */
case class ItemOperation(id: String)

trait Message
case class PlayerMove(x: Int, y: Int) extends Message
case object Activate extends Message
case object Deactivate extends Message
case object Tick extends Message
case class IsOpaque(var opaque: Boolean = false) extends Message
case class IsWalkable(var walkable: Boolean = true) extends Message
case class PickUp(var ok: Boolean = true) extends Message

trait Behavior {
  def receive(state: GameState, self: Item, message: Message): Unit
}

object Behavior {
  import cats.syntax.functor._
  import io.circe.Decoder
  import io.circe.generic.extras.auto._
  import io.circe.generic.extras.Configuration
  implicit private val configuration: Configuration = Configuration.default.withDefaults

  val decoders: Map[String, Decoder[Behavior]] = Map(
    "MotionSensor" -> Decoder[MotionSensor].widen,
    "DoorOpener" -> Decoder[DoorOpener].widen,
    "Affixed" -> Decoder[Affixed].widen,
  )
}

case class MotionSensor(radius: Int, timer: Int = 6) extends Behavior {
  private var activeTicks = 0

  override def receive(state: GameState, self: Item, message: Message): Unit = message match {
    case PlayerMove(x, y) =>
      val loc = state.items.lookup(self)
      loc match {
        case OnFloor(ix, iy) =>
          if ((x - ix).abs + (y - iy).abs <= radius) {
            if (activeTicks == 0)
              state.broadcastToLocation(loc, Activate)
            activeTicks = timer
          }
        case _ =>
      }
    case Tick if activeTicks > 0 =>
      activeTicks -= 1
      if (activeTicks == 0) {
        state.broadcastToLocation(state.items.lookup(self), Deactivate)
      }
    case _ =>
  }
}

case class DoorOpener() extends Behavior {
  var isOpen: Boolean = false

  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case Activate if !isOpen =>
      isOpen = true
    case Deactivate =>
      isOpen = false
    case msg: IsOpaque =>
      msg.opaque = !isOpen
    case msg: IsWalkable =>
      msg.walkable = isOpen
    case _ =>
  }
}

case class Affixed() extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case p: PickUp => p.ok = false
    case _ =>
  }
}
