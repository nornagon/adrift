package adrift.items

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
  affixed: Boolean,
)

trait ItemCondition {
  def functional: Boolean = false
}

case class Item(
  kind: ItemKind,
  conditions: mutable.Buffer[ItemCondition],
  parts: Seq[Item]
) {
  def functional: Boolean = conditions.forall(_.functional) && parts.forall(_.functional)
  val id = Item.nextId
  override def hashCode(): Int = id
  override def equals(obj: Any): Boolean = obj.isInstanceOf[Item] && obj.asInstanceOf[Item].id == id

  override def toString: String = s"Item(id=$id, kind=${kind.name})"
}
object Item {
  private var _nextId = 1
  def nextId: Int = { _nextId += 1; _nextId }
  val item_display: Map[String, Int] = Map(
    "DOOR" -> 0x2B,             // +
    "SMALL_TOOL" -> 0x74,       // t
    "TOOL" -> 0x54 ,            // T
    "RAW_THIN" -> 0x09 ,        // ○ 
    "RAW_ROD" -> 0x7C ,         // |
    "RAW_BLOCK" -> 0x23 ,       // #
    "RAW_PLATE" -> 0x04 ,       // ♦
    "RAW_CLOTH" -> 0xB0 ,       // ░
    "OPTICAL" -> 0x28 ,         // (
    "SMALL_BOX" -> 0x6E ,       // n
    "MEDIUM_BOX" -> 0xEF ,      // ∩
    "LARGE_BOX" -> 0x55 ,       // U
    "SMALL_PLATE" -> 0xA9 ,     // ⌐
    "MEDIUM_PLATE" -> 0xE2 ,    // Γ
    "LARGE_PLATE" -> 0x4C ,     // L
    "SMALL_COMPONENT" -> 0xFA , // ·
    "MEDIUM_COMPONENT" -> 0xF9 ,// ∙
    "LARGE_COMPONENT" -> 0xF8 , // °
    "SMALL_ROD" -> 0x27 ,       // '
    "MEDIUM_ROD" -> 0x21 ,      // !
    "LARGE_ROD" -> 0x2F ,       // /
    "SMALL_PART" -> 0x87 ,      // ç
    "SMALL_EQUIPMENT" -> 0x2D , // -
    "MEDIUM_EQUIPMENT" -> 0x3D ,// =
    "LARGE_EQUIPMENT" -> 0xF0 , // ≡
    "HUMAN_EQUIPMENT" -> 0xE9 , // Θ
    "CLOTHING" -> 0x43,         // C
    "CHAIR" -> 0xD2,            // ╥
    "DESK" -> 0xD1,             // ╤
  )
}


case class Charge(kwh: Double, maxKwh: Double) extends ItemCondition {
  override def functional: Boolean = kwh > 0
}
case class BrokenWire() extends ItemCondition
case class Cracked() extends ItemCondition
case class BurntOut() extends ItemCondition
case class Rusted() extends ItemCondition
case class DoorOpen() extends ItemCondition

case class ItemOperation(id: String)
