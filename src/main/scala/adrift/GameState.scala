package adrift

import adrift.Action._
import adrift.items.{DoorOpen, Item, ItemKind}

import scala.collection.mutable

case class Container(maxItems: Int, contents: mutable.Buffer[Item] = mutable.Buffer.empty)

sealed trait ItemLocation
case class OnFloor(x: Int, y: Int) extends ItemLocation
case class InHands() extends ItemLocation
case class Inside(other: Item) extends ItemLocation
case class Worn() extends ItemLocation

class ItemDatabase {
  private val locationsByItem = mutable.Map.empty[Item, ItemLocation]
  private val itemsByLocation = mutable.Map.empty[ItemLocation, Seq[Item]].withDefault(_ => Seq.empty[Item])

  def put(item: Item, location: ItemLocation): Unit = {
    itemsByLocation(location) :+= item
    locationsByItem(item) = location
  }

  def delete(item: Item): Unit = {
    val loc = lookup(item)
    locationsByItem -= item
    itemsByLocation(loc) = itemsByLocation(loc).filter(_ != item)
  }

  def lookup(item: Item): ItemLocation = {
    locationsByItem(item)
  }
  def lookup(location: ItemLocation): Seq[Item] = {
    itemsByLocation(location)
  }

  def move(item: Item, location: ItemLocation): Unit = {
    delete(item)
    put(item, location)
  }
}


class GameState(data: Data, width: Int, height: Int) {
  val map: Grid[Terrain] = new Grid[Terrain](width, height)(data.terrain("empty space"))
  val items: ItemDatabase = new ItemDatabase
  var player: (Int, Int) = (0, 0)
  val hands = Container(maxItems = 2)

  var message: Option[String] = None

  def receive(action: Action): Unit = {
    message = None
    action match {
      case PlayerMove(dx, dy) =>
        if (canWalk(player._1 + dx, player._2 + dy)) {
          movePlayer(player._1 + dx, player._2 + dy)
        }

      case Disassemble(item) =>
        items.delete(item)
        for (p <- item.parts) items.put(p, OnFloor(player._1, player._2))
        message = Some(s"You take apart the ${item.kind.name}.")

      case Assemble(itemKind, components) =>
        components.foreach(items.delete)
        val newItem = Item(
          kind = itemKind,
          conditions = mutable.Buffer.empty,
          components
        )
        items.put(newItem, OnFloor(player._1, player._2))

      case PickUp(item) =>
        if (item.kind.affixed) {
          message = Some("You can't pick that up.")
        } else if (hands.contents.size < hands.maxItems) {
          items.move(item, InHands())
          hands.contents.append(item)
          message = Some(s"You pick up the ${item.kind.name}.")
        } else {
          message = Some("Your hands are full.")
        }

      case PutDown(item) =>
        items.lookup(item) match {
          case InHands() =>
            items.move(item, OnFloor(player._1, player._2))
            hands.contents.remove(hands.contents.indexOf(item))
            message = Some(s"You place the ${item.kind.name} on the ${map(player).name}.")
          case _ =>
            message = Some("You can't put that down.")
        }
      case Quit =>
    }
  }

  def movePlayer(x: Int, y: Int): Unit = {
    player = (x, y)
    openNearbyDoors()
    recalculateFOV()
  }

  def openNearbyDoors(): Unit = {
    for (x <- player._1 - 6 to player._1 + 6; y <- player._2 - 6 to player._2 + 6) {
      for (i <- items.lookup(OnFloor(x, y)); if i.kind.name == "automatic door") {
        val isOpen = i.conditions.exists(_.isInstanceOf[DoorOpen])
        if (!isOpen && (x - player._1).abs < 2 && (y - player._2).abs < 2) {
          i.conditions.append(DoorOpen())
        } else if (isOpen && ((x - player._1).abs > 2 || (y - player._2).abs > 2)) {
          i.conditions.remove(i.conditions.indexWhere(_.isInstanceOf[DoorOpen]))
        }
      }
    }
  }

  def canWalk(x: Int, y: Int): Boolean = {
    map.get(x, y).exists(_.walkable)// && items(x, y).forall(_.walkable)
  }

  def itemIsOpaque(item: Item): Boolean = {
    item.kind.name match {
      case "automatic door" =>
        !item.conditions.exists(_.isInstanceOf[DoorOpen])
      case _ =>
        false
    }
  }

  def isOpaque(x: Int, y: Int): Boolean = {
    map.get(x, y).exists(_.opaque) || items.lookup(OnFloor(x, y)).exists(itemIsOpaque)
  }

  private var visible = Set.empty[(Int, Int)]
  private def recalculateFOV(): Unit = {
    val newVisible = mutable.Set.empty[(Int, Int)]
    newVisible += player
    val opaque = (dx: Int, dy: Int) => isOpaque(player._1 + dx, player._2 + dy)
    FOV.castShadows(radius = 100, opaqueApply = true, opaque, (x, y) => {
      newVisible.add((player._1 + x, player._2 + y))
    })
    visible = newVisible.toSet
  }

  def isVisible(x: Int, y: Int): Boolean = isVisible((x, y))
  def isVisible(p: (Int, Int)): Boolean = visible contains p

  def nearbyItems: Seq[Item] = {
    val nearbyItems = mutable.Buffer.empty[Item]
    for (dy <- -2 to 2; dx <- -2 to 2) {
      val is = items.lookup(OnFloor(player._1 + dx, player._2 + dy))
      nearbyItems ++= is
    }
    nearbyItems ++ hands.contents
  }

  def buildableItems2(availableItems: Seq[Item]): Seq[(ItemKind, Seq[Item])] = {
    def isBuildable(kind: ItemKind): Option[Seq[Item]] = {
      val partsByKind = kind.parts.groupBy(_._1._1).map {
        case (partKind, xs) =>
          val qty = xs.map(_._1._2).sum
          val parts = availableItems.filter(_.kind == partKind).take(qty)
          val ops = xs.map(_._2).distinct
          if (parts.size == qty && ops.forall(op => availableItems.exists(i => i.kind.provides.contains(op))))
            Some(parts)
          else None
      }.toSeq
      if (partsByKind.forall(_.nonEmpty))
        Some(partsByKind.flatten.flatten)
      else
        None
    }
    (for (kind <- data.items.values; if kind.parts.nonEmpty; parts <- isBuildable(kind)) yield (kind, parts))(collection.breakOut)
  }

  def buildableItems(availableItems: Seq[Item]): Seq[(ItemKind, Seq[Item])] = {
    /*
    // First put together a list of operations we can do with the tools in our area
    var availableOps: Seq[ItemOperation] = Seq()
    for ((item, _) <- availableItems) availableOps ++= item.kind.provides
    // Make a map of the available item kinds and quantities
    val itemIndex: mutable.Map[ItemKind, Seq[ItemLocation]] = mutable.Map()
    availableItems foreach {
      case (item, location) =>
        if (itemIndex.contains(item.kind)) {
          itemIndex(item.kind) = itemIndex(item.kind) :+ location
        } else {
          itemIndex(item.kind) = Seq(location)
        }
    }

    def buildable(itemKind: ItemKind, itemIndex: mutable.Map[ItemKind, Seq[ItemLocation]]): Option[Seq[ItemLocation]] = {
      if (itemIndex.contains(itemKind)) {
        val itemLocations = itemIndex(itemKind)
        if (itemLocations.nonEmpty) {
          val location = itemIndex(itemKind).head
          itemIndex(itemKind) = itemIndex(itemKind).tail
          return Some(Seq(location))
        } else {
          return None
        }
      }
      if (itemKind.parts.isEmpty) return None
      var locs = Seq.empty[ItemLocation]
      // Call this function recursively on the parts of the item to see if each subpart is buildable
      for (((kind: ItemKind, qty: Int), op: ItemOperation) <- itemKind.parts) {
        if (availableOps.contains(op)) {
          var q = qty
          while (q > 0) {
            buildable(kind, itemIndex) match {
              case Some(componentLocations) =>
                locs ++= componentLocations
                q -= 1
              case None =>
                return None
            }
          }
        } else {
          return None
        }
      }
      Some(locs)
    }
    data.items.values.toSeq
      .map { kind => (kind, buildable(kind, mutable.Map.empty ++ itemIndex)) }
      .collect {
        case (kind, Some(locations)) => (kind, locations)
      }
      */
    ???
  }
}
