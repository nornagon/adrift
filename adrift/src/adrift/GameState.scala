package adrift

import adrift.items._
import adrift.items.behaviors.{PartiallyDisassembled, Tool}

import scala.collection.mutable
import scala.util.Random
import RandomImplicits._

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

  def exists(item: Item): Boolean = locationsByItem.contains(item)

  def all: Iterable[Item] = locationsByItem.keys

  def move(item: Item, location: ItemLocation): Unit = {
    delete(item)
    put(item, location)
  }
}

case class Circuit(name: String, max: Int, var stored: Int) {
  def add(amount: Int): Unit = stored = math.min(max, stored + amount)
}

class GameState(val data: Data, width: Int, height: Int, random: Random) {
  val terrain: Grid[Terrain] = new Grid[Terrain](width, height)(data.terrain("empty space"))
  val temperature: Grid[Double] = new Grid[Double](width, height)(random.between(250d, 280d))
  val items: ItemDatabase = new ItemDatabase
  var player: (Int, Int) = (0, 0)
  var bodyTemp: Double = 310

  var walkThroughWalls = false
  var showTempDebug = false

  lazy val circuits: mutable.Map[String, Circuit] = mutable.Map.empty[String, Circuit].withDefault { k =>
    val c = Circuit(k, 500, 500)
    circuits(k) = c
    c
  }

  var message: Option[String] = None

  def receive(action: Action): Unit = {
    message = None
    action match {
      case Action.PlayerMove(dx, dy) =>
        if (canWalk(player._1 + dx, player._2 + dy) || walkThroughWalls) {
          movePlayer(player._1 + dx, player._2 + dy)
        }

      case Action.Disassemble(item) =>
        var anyRemoved = false
        item.parts = item.parts.filter { p =>
          val disassembleOp = item.kind.parts.find(_._1._1 == p.kind).get._2
          val tool = nearbyItems.find { tool => sendMessage(tool, Message.UseTool(disassembleOp)).ok }
          if (tool.nonEmpty) {
            items.put(p, OnFloor(player._1, player._2))
            anyRemoved = true
          }
          tool.isEmpty
        }

        if (item.parts.isEmpty) {
          items.delete(item)
          message = Some(s"You take apart the ${item.kind.name}.")
        } else if (anyRemoved) {
          item.behaviors.append(PartiallyDisassembled())
          message = Some(s"You weren't able to completely take apart the ${item.kind.name}.")
        } else {
          message = Some(s"You don't have the tools to do that.")
        }

      case Action.Assemble(itemKind, components) =>
        components.foreach(items.delete)
        val newItem = Item(
          kind = itemKind,
          parts = components,
          behaviors = mutable.Buffer.empty
        )
        items.put(newItem, OnFloor(player._1, player._2))
        message = Some(s"You make a ${newItem.kind.name}.")

      case Action.PickUp(item) =>
        if (!sendMessage(item, Message.PickUp()).ok) {
          message = Some("You can't pick that up.")
        } else if (items.lookup(InHands()).size >= 2) {
          message = Some("Your hands are full.")
        } else {
          val pickedUpItem = sendMessage(item, Message.PickedUp(item)).item
          items.move(pickedUpItem, InHands())
          message = Some(s"You pick up the ${pickedUpItem.kind.name}.")
        }

      case Action.PutDown(item) =>
        items.lookup(item) match {
          case InHands() =>
            items.move(item, OnFloor(player._1, player._2))
            sendMessage(item, Message.Dropped())
            message = Some(s"You place the ${item.kind.name} on the ${terrain(player).name}.")
          case _ =>
            message = Some("You can't put that down.")
        }

      case Action.Plug(item, into) =>
        items.lookup(item) match {
          case InHands() =>
            sendMessage(item, Message.PlugInto(into))
            message = Some(s"You plug the ${itemDisplayName(item)} into the ${itemDisplayName(into)}.")
          case _ =>
            message = Some("You need to pick it up first.")
        }

      case Action.Quit =>
    }
    items.all.foreach(sendMessage(_, Message.Tick))
    circuits.values.foreach { c => c.stored = math.max(0, c.stored - 100) }
    recalculateFOV()
    updateHeat()
  }

  def sendMessage[Msg <: Message](item: Item, message: Msg): Msg = {
    // copy the behavior list so that behaviors removed during processing won't affect iteration
    val behaviorsCopy = Seq.empty ++ item.behaviors
    for (b <- behaviorsCopy) {
      b.receive(this, item, message)
    }
    message
  }

  def broadcastToLocation(location: ItemLocation, message: Message): Unit = {
    items.lookup(location).foreach(sendMessage(_, message))
  }

  def broadcastToParts[Msg <: Message](item: Item, message: Msg): Msg = {
    item.parts.foreach(sendMessage(_, message))
    message
  }

  def itemDisplayName(item: Item): String = {
    var name = item.kind.name
    val conditions = sendMessage(item, Message.VisibleConditions()).conditions
    if (conditions.nonEmpty)
      name += s" (${conditions.mkString(", ")})"
    name
  }


  def movePlayer(x: Int, y: Int): Unit = {
    val oldPos = player
    player = (x, y)
    items.lookup(InHands()).foreach(sendMessage(_, Message.Hauled(from = oldPos, to = player)))
    broadcastPlayerMoved()
  }

  def broadcastPlayerMoved(): Unit = {
    for {
      y <- player._2 - 2 to player._2 + 2
      x <- player._1 - 2 to player._1 + 2
      if isVisible(x, y)
    } broadcastToLocation(OnFloor(x, y), Message.PlayerMove(player._1, player._2))
  }

  def smash(p: Item): Unit = {
    if (p.parts.isEmpty) {
      if (random.nextFloat() < 0.1) {
        //p.conditions.append(Broken)
      }
    } else {
      // a case should protect its insides from smashing
      // bigger parts should be smashed first
      // delicate components should be damaged more easily
      p.parts.foreach(smash)
    }
  }

  def itemIsOpaque(item: Item): Boolean =
    sendMessage(item, Message.IsOpaque()).opaque

  def itemIsWalkable(item: Item): Boolean =
    sendMessage(item, Message.IsWalkable()).walkable

  def canWalk(x: Int, y: Int): Boolean = {
    terrain.get(x, y).exists(_.walkable) && items.lookup(OnFloor(x, y)).forall(itemIsWalkable)
  }

  def isOpaque(x: Int, y: Int): Boolean = {
    terrain.get(x, y).exists(_.opaque) || items.lookup(OnFloor(x, y)).exists(itemIsOpaque)
  }

  private var visible = Set.empty[(Int, Int)]
  def recalculateFOV(): Unit = {
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
    val onFloor = for {
      dy <- -2 to 2
      dx <- -2 to 2
      loc = OnFloor(player._1 + dx, player._2 + dy)
      if isVisible(loc.x, loc.y)
      i <- items.lookup(loc)
    } yield i
    onFloor ++ items.lookup(InHands())
  }

  def buildableItems2(availableItems: Seq[Item]): Seq[(ItemKind, Seq[Item])] = {
    def isBuildable(kind: ItemKind): Option[Seq[Item]] = {
      val partsByKind = kind.parts.groupBy(_._1._1).map {
        case (partKind, xs) =>
          val qty = xs.map(_._1._2).sum
          val parts = availableItems.filter(_.kind == partKind).take(qty)
          val ops = xs.map(_._2).distinct
          if (parts.size == qty && ops.forall(op => availableItems.exists(_.behaviors.exists { case t: Tool if t.op == op.id => true; case _ => false })))
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

  def moveHeat(dt: Double, a: (Int, Int), b: (Int, Int)): Unit = {
    val terA = terrain(a)
    val terB = terrain(b)
    val ta = temperature(a)
    val tb = temperature(b)
    val k = terA.heatTransfer * terB.heatTransfer
    val w = (ta - tb) * k
    val dq = w * dt
    if (temperature.contains(a)) temperature(a) -= dq / terA.heatCapacity
    if (temperature.contains(b)) temperature(b) += dq / terB.heatCapacity
  }

  def updateHeat(dt: Double = 0.05): Unit = {
    import RandomImplicits._
    def randomAdj(p: (Int, Int)): (Int, Int) = {
      val (x, y) = p
      random.between(0, 4) match {
        case 0 => (x - 1, y)
        case 1 => (x + 1, y)
        case 2 => (x, y - 1)
        case 3 => (x, y + 1)
      }
    }
    for (_ <- 0 until (width * height * 0.4).round.toInt) {
      val a = (random.between(0, width), random.between(0, height))
      val b = randomAdj(a)
      if (temperature.contains(a) && temperature.contains(b))
        moveHeat(dt, a, b)
      if (terrain(a).name == "floor" && random.oneIn(2)) {
        var p = a
        for (_ <- 1 to random.between(2, 8)) {
          val test = randomAdj(p)
          if (canWalk(test._1, test._2)) {
            p = test
          }
        }
        if (p != a) {
          val tmp = temperature(p)
          temperature(p) = temperature(a)
          temperature(a) = tmp
        }
      }
    }

    val playerHeatCapacity = 4
    val playerTileTemp = temperature(player)
    val k = 0.01
    val w = (bodyTemp - playerTileTemp) * k
    val dq = w * dt
    bodyTemp -= dq / playerHeatCapacity
    temperature(player) += dq / terrain(player).heatCapacity
  }
}
