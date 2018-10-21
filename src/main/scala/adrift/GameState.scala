package adrift

import adrift.Action._
import adrift.items.{DoorOpen, Item}
import adrift.items.ItemKind
import adrift.items.ItemOperation

import scala.collection.mutable

sealed trait ItemLocation
case class OnFloor(x: Int, y: Int/* TODO: level? */, index: Int) extends ItemLocation
case class InHands(index: Int) extends ItemLocation


case class Container(maxItems: Int, contents: mutable.Buffer[Item] = mutable.Buffer.empty)


class GameState(data: Data, width: Int, height: Int) {
  val map: Grid[Terrain] = new Grid[Terrain](width, height)(data.terrain("empty space"))
  val items: Grid[Seq[Item]] = new Grid[Seq[Item]](width, height)(Seq.empty)
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
      case Disassemble(location) =>
        val item = removeItem(location)
        items(player) ++= item.parts
        message = Some(s"You take apart the ${item.kind.name}.")
      case Assemble(item,location) => {
        // if item in buildableItems() {   // handle in ui?
          items(player) :+= item
        // }
      }
      case PickUp(location) =>
        if (itemAtLocation(location).kind.affixed) {
          message = Some("You can't pick that up.")
        } else if (hands.contents.size < hands.maxItems) {
          val item = removeItem(location)
          hands.contents.append(item)
          message = Some(s"You pick up the ${item.kind.name}.")
        } else {
          message = Some("Your hands are full.")
        }
      case PutDown(location) =>
        location match {
          case loc: InHands =>
            val item = removeItem(loc)
            items(player) :+= item
            message = Some(s"You place the ${item.kind.name} on the ${map(player)}.")
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
      for (i <- items(x, y); if i.kind.name == "automatic door") {
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
    map.get(x, y).exists(_.opaque) || items(x, y).exists(itemIsOpaque)
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

  def itemAtLocation(location: ItemLocation): Item = location match {
    case OnFloor(x, y, index) =>
      items(x, y)(index)
    case InHands(index) =>
      hands.contents(index)
  }

  def removeItem(location: ItemLocation): Item = location match {
    case OnFloor(x, y, index) =>
      val item = items(x, y)(index)
      items(x, y) = items(x, y).patch(index, Nil, 1)
      item
    case InHands(index) =>
      val item = hands.contents(index)
      hands.contents.remove(index)
      item
  }

  def addItem(item: Item, location: OnFloor) = {
    items(location.x, location.y) :+ item
  }

  def nearbyItems: Seq[(Item, ItemLocation)] = {
    val nearbyItems = mutable.Buffer.empty[(Item, ItemLocation)]
    for (dy <- -2 to 2; dx <- -2 to 2) {
      val is = items(player._1 + dx, player._2 + dy)
      if (is.nonEmpty) {
        nearbyItems ++= is.zipWithIndex.map { case (item, i) => (item, OnFloor(player._1 + dx, player._2 + dy, i)) }
      }
    }
    nearbyItems ++ inHandItems
  }

  def inHandItems: Seq[(Item, ItemLocation)] = {
    hands.contents.zipWithIndex.map {
      case (item, i) => (item, InHands(i))
    }
  }


  def buildableItems(availableItems: Seq[Item]): Seq[ItemKind] = {
    // First put together a list of operations we can do with the tools in our area
    var availableOps: Seq[ItemOperation] = Seq()
    for (item <- availableItems) availableOps ++= item.kind.provides
    // Make a map of the available item kinds and quantities
    var itemIndex: mutable.Map[ItemKind, Int] = mutable.Map()
    availableItems foreach {
      case (item) => {
        if (itemIndex.contains(item.kind)) {
          val qty: Int = itemIndex(item.kind) + 1
          itemIndex = itemIndex + (item.kind -> qty)
        } else {
          itemIndex = itemIndex + (item.kind -> 1)
        }
      }
    }

    def buildable(item: ItemKind, itemIndex: mutable.Map[ItemKind, Int]): Boolean = {
      if (itemIndex.contains(item)) {
        val qty = itemIndex(item)
        if (qty > 0) {
          itemIndex += (item -> (qty - 1))
          return true
        } else {
          return false
        }
      }
      // Call this function recursively on the parts of the item to see if each subpart is buildable
      for (((kind: ItemKind, qty: Int), op: ItemOperation) <- item.parts) {
        if (availableOps.contains(op)) {
          var q = qty
          while (q > 0) {
            if (buildable(kind, itemIndex)) {
              q = q - 1
            } else {
              return false
            }
          }
        } else {
          return false
        }
      }
      true
    }
    data.items.values.toSeq.filter(itemkind => buildable(itemkind, itemIndex))
  }
}
