package adrift

import adrift.Action._
import adrift.items.Item

import scala.collection.mutable

sealed trait ItemLocation
case class OnFloor(x: Int, y: Int/* TODO: level? */, index: Int) extends ItemLocation
case class InHands(index: Int) extends ItemLocation


case class Container(maxItems: Int, contents: mutable.Buffer[Item] = mutable.Buffer.empty)


class GameState(width: Int, height: Int) {
  val map: Grid[Terrain] = new Grid[Terrain](width, height)(Terrain.EmptySpace)
  val furniture: Grid[Option[Furniture]] = new Grid[Option[Furniture]](width, height)(None)
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
      case Assemble(item) => {
        // if item in buildableItems() {   // handle in ui?
          items(player) :+= item
        // }
      }
      case PickUp(location) =>
        if (hands.contents.size < hands.maxItems) {
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
      for (f <- furniture.get(x, y).flatten) {
        f match {
          case door: Furniture.AutomaticDoor =>
            if (!door.open && (x - player._1).abs < 2 && (y - player._2).abs < 2) {
              door.open = true
            } else if (door.open && ((x - player._1).abs > 2 || (y - player._2).abs > 2)) {
              door.open = false
            }
          case _ =>
        }
      }
    }
  }

  def canWalk(x: Int, y: Int): Boolean = {
    map.get(x, y).exists(_.walkable) && furniture.get(x, y).flatten.forall(_.walkable)
  }

  def isOpaque(x: Int, y: Int): Boolean = {
    map.get(x, y).exists(_.opaque) || furniture.get(x, y).flatten.exists(_.opaque)
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
  def buildableItems(availableItems: Seq[Item]): Seq[Item] = {
    val availableOps: Seq(Operation) = Seq()
    for (item <- availableItems) match {
      case t: Tool() =>  availableOps :+ t.provides()
      case _ => 
    }
    var itemIndex: Map(ItemKind,Int) = Map()
    foreach (item<-availableItems) {
      if itemIndex.keys.contains(item.kind) {
        itemIndex(item.kind) += 1
      } else {
        itemIndex = itemIndex + (item.kind -> 1)
      }
    }
    val constructable: Seq(ItemKind) = Data.items.values.filter(i => i.parts.len>0)
    def buildable(item: ItemKind, available: Map(ItemKind,Int)): Boolean = {
      // Call this function recursively on the parts of the item
      if item 

      item.parts.flatmap(((kind,qty),op) => {
        if availableOps.contains(op) {
          if available.keys.contains(kind)  
        }
        })
    }
  }

}
