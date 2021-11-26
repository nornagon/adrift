package adrift

import adrift.items.Item

import scala.collection.mutable

sealed trait ItemLocation
case class OnFloor(l: Location) extends ItemLocation
case class InHands() extends ItemLocation
case class Inside(other: Item) extends ItemLocation
case class Worn() extends ItemLocation

class ItemDatabase {
  private val locationsByItem = mutable.Map.empty[Item, ItemLocation]
  private val itemsByLocation = mutable.Map.empty[ItemLocation, Seq[Item]].withDefault(_ => Seq.empty[Item])
  private val tickableItems = mutable.Set.empty[Item]

  def put(item: Item, location: ItemLocation): Unit = {
    itemsByLocation(location) :+= item
    locationsByItem(item) = location
    if (item.tickable) tickableItems += item
  }

  def delete(item: Item): Unit = {
    val loc = lookup(item)
    locationsByItem -= item
    itemsByLocation(loc) = itemsByLocation(loc).filter(_ != item)
    tickableItems -= item
  }

  def lookup(item: Item): ItemLocation = locationsByItem(item)
  def lookup(location: ItemLocation): Seq[Item] = itemsByLocation(location)

  def exists(item: Item): Boolean = locationsByItem.contains(item)

  def all: Iterable[Item] = locationsByItem.keys
  def tickable: Iterable[Item] = tickableItems

  def move(item: Item, location: ItemLocation): Unit = {
    delete(item)
    put(item, location)
  }
}
