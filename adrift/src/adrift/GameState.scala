package adrift

import adrift.Action.AssemblyAction
import adrift.RandomImplicits._
import adrift.display.Appearance
import adrift.items.Message.{IsFunctional, PlayerBump, Provides}
import adrift.items._
import adrift.items.behaviors.{Opened, PartiallyDisassembled}

import scala.collection.mutable
import scala.util.Random

case class Circuit(name: String, max: Int, var stored: Int) {
  def add(amount: Int): Unit = stored = math.min(max, stored + amount)
}

case class LevelId(id: String) extends AnyVal
case class Location(levelId: LevelId, x: Int, y: Int) {
  def +(dx: Int, dy: Int): Location = copy(levelId, x + dx, y + dy)
  def xy: (Int, Int) = (x, y)
}
object Location {
  def apply(levelId: LevelId, pos: (Int, Int)): Location = Location(levelId, pos._1, pos._2)
}

case class Level(
  var terrain: Grid[Terrain],
  var temperature: Grid[Double],
  var gasComposition: Grid[GasComposition],
) {
  val width: Int = terrain.width
  val height: Int = terrain.height
  assert(width == temperature.width)
  assert(width == gasComposition.width)
  assert(height == temperature.height)
  assert(height == gasComposition.height)

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

  def moveGas(dt: Double, a: (Int, Int), b: (Int, Int)): Unit = {
    val gca = gasComposition(a)
    val gcb = gasComposition(b)
    val w = (gca - gcb) * 0.5
    val dpp = w * dt
    gasComposition(a) -= dpp
    gasComposition(b) += dpp
  }

  def updateHeat(dt: Double = 1, isPermeable: (Int, Int) => Boolean)(implicit random: Random): Unit = {
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
      // diffusion
      val a = (random.between(0, width), random.between(0, height))
      val b = randomAdj(a)
      if (temperature.contains(a) && temperature.contains(b)) {
        moveHeat(dt / 20, a, b)
        if (isPermeable(a._1, a._2) && isPermeable(b._1, b._2)) {
          moveGas(dt, a, b)
        }
      }

      // convection
      if (terrain(a).name == "floor" && random.oneIn(2)) {
        var p = a
        for (_ <- 1 to random.between(2, 8)) {
          val test = randomAdj(p)
          val (x, y) = test
          if (y >= 0 && y < height && isPermeable(x, y)) {
            p = test
          }
        }
        if (p != a) {
          val tmp = temperature(p)
          temperature(p) = temperature(a)
          temperature(a) = tmp
          val tmp2 = gasComposition(p)
          gasComposition(p) = gasComposition(a)
          gasComposition(a) = tmp2
        }
      }
    }
  }
}
object Level {
  def emptyCylinder(data: Data, width: Int, height: Int)(implicit random: Random): Level =
    Level(
      terrain = new CylinderGrid(width, height)(data.terrain("floor")),
      temperature = new CylinderGrid(width, height)(random.between(250d, 270d)),
      gasComposition = new CylinderGrid(width, height)(GasComposition(4, 9, 1))
    )
  def emptySquare(data: Data, width: Int, height: Int)(implicit random: Random): Level =
    Level(
      terrain = new Grid(width, height)(data.terrain("floor")),
      temperature = new Grid(width, height)(random.between(250d, 270d)),
      gasComposition = new Grid(width, height)(GasComposition(4, 9, 1))
    )
}

class GameState(var data: Data, val random: Random) {
  var levels = mutable.Map.empty[LevelId, Level]
  var itemDb: ItemDatabase = new ItemDatabase
  var player: Location = Location(LevelId("main"), 0, 0)
  var bodyTemp: Double = 310
  var internalCalories: Int = 8000
  var currentTime = 0
  // TODO: save the _logical_ display here rather than the physical display
  var mapMemory = mutable.Map.empty[LevelId, Grid[Option[(Char, Color, Color)]]]

  def remembered(loc: Location): Option[(Char, Color, Color)] =
    mapMemory.get(loc.levelId).flatMap(_.getOrElse(loc.xy, None))

  def updateMemory(): Unit = {
    val level = levels(player.levelId)
    val memory = mapMemory.getOrElseUpdate(player.levelId, new Grid[Option[(Char, Color, Color)]](level.width, level.height)(None))
    for ((x, y) <- visible; if memory.contains(x, y))
      memory(x, y) = Some(Appearance.charAtPosition(this, x, y))
  }

  var isRoomTest: Boolean = false

  class NormalizedItemDb() {
    def put(item: Item, location: ItemLocation): Unit = itemDb.put(item, normalize(location))
    def delete(item: Item): Unit = itemDb.delete(item)
    def lookup(item: Item): ItemLocation = itemDb.lookup(item)
    def lookup(location: ItemLocation): Seq[Item] = itemDb.lookup(normalize(location))
    def exists(item: Item): Boolean = itemDb.exists(item)
    def all: Iterable[Item] = itemDb.all
    def move(item: Item, location: ItemLocation): Unit = itemDb.move(item, normalize(location))
  }

  val items = new NormalizedItemDb()

  def normalize(l: Location): Location = {
    val level = levels(l.levelId)
    if (l.x >= 0 && l.x < level.width) return l
    l.copy(x = level.terrain.normalizeX(l.x))
  }
  def normalize(l: ItemLocation): ItemLocation = l match {
    case OnFloor(l) if l.x < 0 || l.x >= levels(l.levelId).width => OnFloor(normalize(l))
    case other => other
  }

  def sightRadius: Int = {
    math.max(1, math.min(internalCalories / 200, 100))
  }

  var deathReason: Option[String] = None
  def isDead: Boolean = deathReason.nonEmpty

  def die(reason: String): Unit = {
    putMessage("You die.")
    deathReason = Some(reason)
  }

  lazy val circuits: mutable.Map[String, Circuit] = mutable.Map.empty[String, Circuit].withDefault { k =>
    val c = Circuit(k, 500, 500)
    circuits(k) = c
    c
  }

  var walkThroughWalls = false
  var seeThroughWalls = false
  var showTempDebug = false
  var showGasDebug = false


  var messages: Seq[(String, Int)] = Seq.empty
  def putMessage(message: String): Unit = messages :+= ((message, currentTime))

  def elapse(durationSec: Int): Unit = {
    for (_ <- 0 until durationSec) {
      items.all.foreach(sendMessage(_, Message.Tick))
      val start = System.nanoTime()
      updateHeat(player.levelId)
      println(f"heat+gas sim took ${(System.nanoTime() - start) / 1e6}%.2f ms")
      checkBodyTemp()
      internalCalories -= 1
      checkHunger()
      currentTime += 1
    }
  }

  def receive(action: Action): Unit = {
    action match {
      case Action.PlayerMove(dx, dy) =>
        if (canWalk(player + (dx, dy)) || walkThroughWalls) {
          movePlayer(player + (dx, dy))
        } else {
          val loc = player + (dx, dy)
          broadcastToLocation(OnFloor(loc), PlayerBump(loc))
        }
        elapse(1)

      case Action.Open(item) =>
        if (sendMessage(item, Message.IsOpened()).opened) {
          putMessage(s"The ${itemDisplayName(item)} is already open.")
        } else {
          elapse(10)
          item.behaviors += Opened()
          putMessage(s"You open the ${itemDisplayName(item)}.")
        }

      case Action.Close(item) =>
        if (!sendMessage(item, Message.IsOpened()).opened) {
          putMessage(s"The ${itemDisplayName(item)} isn't open.")
        } else {
          elapse(10)
          item.behaviors --= item.behaviors.filter(_.isInstanceOf[Opened])
          putMessage(s"You close the ${itemDisplayName(item)}.")
        }

      case Action.Disassemble(item) =>
        var anyRemoved = false
        item.parts = item.parts.filter { p =>
          val disassembleOp = item.kind.parts.find(_.kind == p.kind).get.operation
          if (disassembleOp.id == "HANDLING") {
            items.put(p, OnFloor(player))
            false
          } else {
            val tool = nearbyItems.find { tool => sendMessage(tool, Message.UseTool(disassembleOp)).ok }
            if (tool.nonEmpty) {
              items.put(p, OnFloor(player))
              anyRemoved = true
            }
            tool.isEmpty
          }
        }

        if (item.parts.isEmpty) {
          items.delete(item)
          elapse(10)
          putMessage(s"You take apart the ${itemDisplayName(item)}.")
        } else if (anyRemoved) {
          elapse(10)
          putMessage(s"You weren't able to completely take apart the ${itemDisplayName(item)}.")
          item.behaviors.append(PartiallyDisassembled())
        } else {
          putMessage(s"You don't have the tools to do that.")
        }

      case Action.Assemble(itemKind, ops) =>
        val ok = ops.forall {
          case AssemblyAction(tool, part, op) =>
            elapse(1)
            sendMessage(tool, Message.UseTool(op)).ok
        }
        if (ok) {
          val parts = ops.map(_.part)
          parts.foreach(items.delete)
          val newItem = itemKind.fromParts(parts)
          items.put(newItem, OnFloor(player))
          putMessage(s"You make a ${itemDisplayName(newItem)}.")
        } else {
          putMessage(s"Your tools fail you.")
        }

      case Action.PickUp(item) =>
        if (!sendMessage(item, Message.PickUp()).ok) {
          putMessage("You can't pick that up.")
        } else if (items.lookup(InHands()).size >= 2) {
          putMessage("Your hands are full.")
        } else {
          val pickedUpItem = sendMessage(item, Message.PickedUp(item)).item
          items.move(pickedUpItem, InHands())
          elapse(1)
          putMessage(s"You pick up the ${itemDisplayName(pickedUpItem)}.")
        }

      case Action.PutDown(item) =>
        items.lookup(item) match {
          case InHands() =>
            items.move(item, OnFloor(player))
            sendMessage(item, Message.Dropped())
            elapse(1)
            putMessage(s"You place the ${itemDisplayName(item)} on the ${levels(player.levelId).terrain(player.x, player.y).name}.")
          case _ =>
            putMessage("You can't put that down.")
        }

      case Action.Plug(item, into) =>
        items.lookup(item) match {
          case InHands() =>
            sendMessage(item, Message.PlugInto(into))
            elapse(1)
            putMessage(s"You plug the ${itemDisplayName(item)} into the ${itemDisplayName(into)}.")
          case _ =>
            putMessage("You need to pick it up first.")
        }

      case Action.Wear(item) =>
        items.move(item, Worn())
        elapse(5)
        putMessage(s"You put on the ${itemDisplayName(item)}.")

      case Action.TakeOff(item) =>
        items.move(item, OnFloor(player))
        sendMessage(item, Message.Dropped())
        elapse(5)
        putMessage(s"You take off the ${itemDisplayName(item)}.")

      case Action.Wait(durationSec) =>
        elapse(durationSec)

      case Action.Eat(item: Item) =>
        if (internalCalories > 10000) {
          putMessage(s"You can't imagine eating another bite.")
        } else {
          if (isEdible(item)) {
            elapse(30)
            eat(item)
            putMessage(s"You finish eating the ${itemDisplayName(item)}.")
          }
        }

      case Action.Quit =>

      case Action.Regenerate =>

      case Action.ReloadData(newData) =>
        println("Reloaded data.")
        data = newData
    }
    recalculateFOV()
  }

  private def checkBodyTemp(): Unit = {
    // https://en.wikipedia.org/wiki/Human_body_temperature#Temperature_variation
    val K = 273
    if (bodyTemp > 35 + K && bodyTemp <= 36 + K) {
      if (random.oneIn(60)) {
        putMessage("You shiver.")
      }
    } else if (bodyTemp > 34 + K && bodyTemp <= 35 + K) {
      if (random.oneIn(30)) {
        putMessage("Your teeth chatter. It's freezing cold.")
      }
    } else if (bodyTemp > 33 + K && bodyTemp <= 34 + K) {
      if (random.oneIn(10)) {
        putMessage("You're so cold you can't even shiver.")
      }
    } else if (bodyTemp <= 33 + K) {
      if (random.oneIn(10)) {
        putMessage("Is it hot in here? No, that can't be right...")
      }
      if (random.oneIn(30)) {
        die("hypothermia")
      }
    }
  }

  private def checkHunger(): Unit = {
    if (internalCalories < 6000) {
      if (random.oneIn(60)) {
        putMessage("Your stomach grumbles.")
      }
    } else if (internalCalories < 5000) {
      if (random.oneIn(30)) {
        putMessage("Your empty stomach aches.")
      }
    } else if (internalCalories < 4000) {
      if (random.oneIn(30)) {
        putMessage("You can't remember when you last ate.")
      }
    } else if (internalCalories < 2000) {
      if (random.oneIn(20)) {
        putMessage("You feel faint with hunger.")
        elapse(1)
      }
    } else if (internalCalories < 1000) {
      if (random.oneIn(10)) {
        putMessage("You feel dizzy with hunger. You have to sit down for a moment.")
        elapse(1)
      }
    }
    if (internalCalories < -8000) {
      die("hunger")
      return
    }
  }

  def sendMessage[Msg <: Message](item: Item, message: Msg): Msg = {
    // copy the behavior list so that behaviors removed during processing won't affect iteration
    val behaviorsCopy = Seq.empty ++ item.behaviors
    for (b <- behaviorsCopy) {
      b.receive(this, item, message)
    }
    message
  }

  def broadcastToLocation[Msg <: Message](location: ItemLocation, message: Msg): Msg = {
    items.lookup(location).foreach(sendMessage(_, message))
    message
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

  def getItemTile(item: Item): Location = {
    items.lookup(item) match {
      case OnFloor(l) => l
      case InHands() | Worn() => player
      case Inside(other) => getItemTile(other)
    }
  }

  def sampleItem(table: Population.Table[String]): Seq[Item] = {
    for {
      itemKindName <- table.sample()(random, data.itemGroups.view.mapValues(_.choose))
    } yield {
      data.items.get(itemKindName) match {
        case Some(itemKind) =>
          itemKind.generateItem()
        case None =>
          throw new RuntimeException(s"""No item with name "$itemKindName"""")
      }
    }
  }

  def movePlayer(l: Location): Unit = {
    val oldPos = player
    player = normalize(l)
    items.lookup(InHands()).foreach(sendMessage(_, Message.Hauled(from = oldPos, to = player)))
    broadcastPlayerMoved()
  }

  def broadcastPlayerMoved(): Unit = {
    for {
      dy <- -2 to +2
      dx <- -2 to +2
      loc = player + (dx, dy)
      if isVisible(loc)
    } broadcastToLocation(OnFloor(loc), Message.PlayerMove(player))
  }

  def isFunctional(p: Item): Boolean =
    sendMessage(p, IsFunctional()).functional && p.parts.forall(isFunctional)

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

  def itemIsPermeable(item: Item): Boolean =
    sendMessage(item, Message.IsPermeable()).permeable

  def terrain(l: Location): Option[Terrain] = levels(l.levelId).terrain.get(l.x, l.y)

  def canWalk(l: Location): Boolean =
    terrain(l).exists(_.walkable) && items.lookup(OnFloor(l)).forall(itemIsWalkable)

  def isOpaque(l: Location): Boolean =
    terrain(l).exists(_.opaque) || items.lookup(OnFloor(l)).exists(itemIsOpaque)

  private val isPermeableCache = mutable.Map.empty[LevelId, CylinderGrid[Boolean]]
  private val dirtyPermeabilityTiles: mutable.Queue[Location] = mutable.Queue.empty[Location]
  def recomputePermeabilityCache(): Unit = {
    isPermeableCache.clear()
    for ((lId, l) <- levels) {
      val g = new CylinderGrid[Boolean](l.width, l.height)(true)
      for (y <- 0 until l.height; x <- 0 until l.width) {
        g(x, y) = isPermeable(Location(lId, x, y))
      }
      isPermeableCache(lId) = g
    }
  }
  def markPermeabilityDirty(location: Location): Unit = dirtyPermeabilityTiles.enqueue(location)
  def processPermeabilityUpdateQueue(): Unit = {
    while (dirtyPermeabilityTiles.nonEmpty) {
      val l = dirtyPermeabilityTiles.dequeue()
      isPermeableCache(l.levelId)(l.x, l.y) = isPermeable(l)
    }
  }

  def refresh(): Unit = {
    recomputePermeabilityCache()
  }

  def isPermeable(l: Location): Boolean =
    terrain(l).exists(_.permeable) && items.lookup(OnFloor(l)).forall(itemIsPermeable)

  def isEdible(item: Item): Boolean = sendMessage(item, Message.IsEdible()).edible
  def eat(item: Item): Unit =
    internalCalories += sendMessage(item, Message.Eat()).calories

  private var visible = Set.empty[(Int, Int)]
  def recalculateFOV(): Unit = {
    val newVisible = mutable.Set.empty[(Int, Int)]
    newVisible += ((player.x, player.y))
    val opaque = (dx: Int, dy: Int) => !seeThroughWalls && isOpaque(player + (dx, dy))
    val level = levels(player.levelId)
    def normalizeX(x: Int): Int = {
      if (x >= 0 && x < level.width) return x
      level.terrain.normalizeX(x)
    }
    FOV.castShadows(radius = sightRadius, opaqueApply = true, opaque, (x, y) => {
      newVisible.add((normalizeX(player.x + x), player.y + y))
    })
    visible = newVisible.toSet
    updateMemory()
  }

  def isVisible(location: Location): Boolean =
    location.levelId == player.levelId && visible.contains(normalize(location).xy)

  def nearbyItems: Seq[Item] = {
    val onFloor = for {
      dy <- -2 to 2
      dx <- -2 to 2
      loc = OnFloor(player + (dx, dy))
      if isVisible(loc.l)
      i <- items.lookup(loc)
    } yield i
    onFloor ++ items.lookup(InHands()) ++ items.lookup(Worn())
  }

  def buildableItems2(availableItems: Seq[Item]): Seq[(ItemKind, Seq[AssemblyAction])] = {
    def isBuildable(kind: ItemKind): Option[Seq[AssemblyAction]] = {
      def providesOperation(operation: ItemOperation, item: Item): Boolean =
        sendMessage(item, Provides(operation)).provides
      def toolsForOp(operation: ItemOperation): Seq[Item] =
        availableItems.filter(providesOperation(operation, _))
      Some(kind.parts.groupBy(_.kind).flatMap {
        case (partKind: ItemKind, parts: Seq[ItemPart]) =>
          val requiredQty = parts.map(_.count).sum
          val candidateComponents = availableItems.filter(_.kind == partKind).take(requiredQty)
          if (candidateComponents.size < requiredQty) return None
          val requiredOps = parts.map(_.operation).distinct
          val candidateTools: Map[ItemOperation, Seq[Item]] =
            requiredOps.view.map { op => op -> toolsForOp(op) }.to(Map)
          if (candidateTools.values.exists(_.isEmpty)) {
            return None
          }
          // NB. parts and tools are assumed to be non-overlapping
          val tools = candidateTools.view.mapValues(_.head)

          parts.flatMap(part => Seq.fill(part.count)(part.operation)).zip(candidateComponents).map {
            case (op, item) =>
              AssemblyAction(tools(op), item, op)
          }

          /*val operations = candidateComponents.zip(parts.map(_.operation)).map {
            case (item, op) => AssemblyAction(tools(op), item, op)
          }*/
      }.to(Seq))
    }

    val x = (for {
      kind <- data.items.values
      if kind.parts.nonEmpty
      operations <- isBuildable(kind)
    } yield (kind, operations)).toSeq
    x
  }

  def updateHeat(levelId: LevelId, dt: Double = 1): Unit = {
    val level = levels(levelId)
    processPermeabilityUpdateQueue()
    val permeability = isPermeableCache(levelId)
    level.updateHeat(dt, (x: Int, y: Int) => permeability(x, y))(random)

    // transfer heat between player & environment
    val playerHeatCapacity = 4 // ~water

    val playerTileTemp = level.temperature(player.x, player.y)
    val k = 0.01
    val w = (bodyTemp - playerTileTemp) * k

    val dq = broadcastToLocation(Worn(), Message.LoseHeat(dq = w * dt / 20)).dq
    bodyTemp -= dq / playerHeatCapacity
    level.temperature(player.x, player.y) += dq / level.terrain(player.x, player.y).heatCapacity

    // player generates heat through metabolism
    // the thought here is:
    // - the body tries to heat itself back up to its base temperature, 310 °K
    // - it can't generate more than a certain amount of heat per time (maxMetabolismDq)
    // - it can't cool the body down (min 0 dq)
    //
    // maxMetabolismDq was experimentally determined by trial and error and
    // a test of "reasonableness" (standing in a cold corridor should cool you
    // down fairly quickly but not too quickly, standing next to a heater
    // should let your body warm back up and stay at 310 °K).
    //
    // TODO: metabolising to produce heat should take calories from stored
    // energy once food is implemented
    val maxMetabolismDq = 0.01
    val metabolismDq = math.max(0, math.min((310 - bodyTemp) * 0.9, maxMetabolismDq))
    bodyTemp += metabolismDq / playerHeatCapacity
  }
}
