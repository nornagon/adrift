package adrift

import adrift.Action.AssemblyAction
import adrift.Population.Table
import adrift.RandomImplicits._
import adrift.YamlObject.ItemWithExtras
import adrift.display.Appearance
import adrift.items.Message.{CanContain, IsFunctional, PlayerBump, Provides}
import adrift.items._
import adrift.items.behaviors.{MissingParts, PartInstalled}

import scala.annotation.tailrec
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
  var powerCables: Grid[Int],
  var dataCables: Grid[Int],
  var fluidCables: Grid[Int],
  var atmosphere: Array[Float],
) {
  val width: Int = terrain.width
  val height: Int = terrain.height
  assert(atmosphere.size == width * height * 4)

  def temperature(xy: (Int, Int)): Float = temperature(xy._1, xy._2)
  def temperature(x: Int, y: Int): Float = atmosphere((y * width + terrain.normalizeX(x)) * 4)
  def setTemperature(x: Int, y: Int, v: Float): Unit = atmosphere((y * width + terrain.normalizeX(x)) * 4) = v
  def gasComposition(xy: (Int, Int)): GasComposition = gasComposition(xy._1, xy._2)
  def gasComposition(x: Int, y: Int): GasComposition = {
    val nx = terrain.normalizeX(x)
    val i = (y * width + nx) * 4
    GasComposition(atmosphere(i + 1), atmosphere(i + 2), atmosphere(i + 3))
  }
  def setGasComposition(x: Int, y: Int, v: GasComposition): Unit = {
    val nx = terrain.normalizeX(x)
    val i = (y * width + nx) * 4
    atmosphere(i + 1) = v.oxygen
    atmosphere(i + 2) = v.carbonDioxide
    atmosphere(i + 3) = v.nitrogen
  }

  private val atmoSim = new AtmoSim(width, height)

  def updateAtmoSimStatics(isPermeable: (Int, Int) => Boolean): Unit = {
    val grid = new Grid[Float](width, height)(0)
    for ((x, y) <- grid.indices) {
      grid(x, y) = terrain(x, y).heatTransfer
    }
    atmoSim.updateTransferTexture(grid, (x, y) => if (isPermeable(x, y)) 1 else 0)
    for ((x, y) <- grid.indices) {
      grid(x, y) = terrain(x, y).heatCapacity
    }
    atmoSim.updateHeatCapacityTexture(grid)
  }

  def updateAtmosphere(dt: Double = 1)(implicit random: Random): Unit = {
    atmoSim.step(atmosphere, cylindrical = terrain.isInstanceOf[CylinderGrid[_]])
  }
}
object Level {
  def emptyCylinder(data: Data, width: Int, height: Int)(implicit random: Random): Level =
    Level(
      terrain = new CylinderGrid(width, height)(data.terrain("floor")),
      powerCables = new CylinderGrid(width, height)(0),
      dataCables = new CylinderGrid(width, height)(0),
      fluidCables = new CylinderGrid(width, height)(0),
      atmosphere = {
        val atmo = new Array[Float](width * height * 4)
        val gc = GasComposition.earthLike
        for (y <- 0 until height; x <- 0 until width; i = (y * width + x) * 4) {
          atmo(i) = random.between(250f, 270f)
          atmo(i + 1) = gc.oxygen
          atmo(i + 2) = gc.carbonDioxide
          atmo(i + 3) = gc.nitrogen
        }
        atmo
      }
    )
  def emptySquare(data: Data, width: Int, height: Int)(implicit random: Random): Level =
    Level(
      terrain = new Grid(width, height)(data.terrain("floor")),
      powerCables = new Grid(width, height)(0),
      dataCables = new Grid(width, height)(0),
      fluidCables = new Grid(width, height)(0),
      atmosphere = {
        val atmo = new Array[Float](width * height * 4)
        val gc = GasComposition.earthLike
        for (y <- 0 until height; x <- 0 until width; i = (y * width + x) * 4) {
          atmo(i) = random.between(250f, 270f)
          atmo(i + 1) = gc.oxygen
          atmo(i + 2) = gc.carbonDioxide
          atmo(i + 3) = gc.nitrogen
        }
        atmo
      }
    )
}

class GameState(var data: Data, val random: Random) {
  var levels = mutable.Map.empty[LevelId, Level]
  var itemDb: ItemDatabase = new ItemDatabase
  var player: Location = Location(LevelId("main"), 0, 0)
  var bodyTemp: Float = 310
  var thermalBodyState: ThermalBodyState = Comfortable
  var breathingBodyState: BreathingBodyState = Normal
  var internalCalories: Int = 8000
  var currentTime = 0
  // TODO: save the _logical_ display here rather than the physical display
  var mapMemory = mutable.Map.empty[LevelId, Grid[Option[(Char, Color, Color)]]]
  val circuits: mutable.Map[String, Circuit] = {
    val m = mutable.Map.empty[String, Circuit]
    m.withDefault { k =>
      val c = Circuit(k, 500, 500)
      m(k) = c
      c
    }
  }
  def bodyStates: Set[BodyState] = Set(thermalBodyState, breathingBodyState)
  def symptoms: Seq[Symptom] = (thermalBodyState.symptoms++breathingBodyState.symptoms).toSeq.sortBy(_.priority)
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
    if (invulnerable) return
    putMessage("You die.")
    deathReason = Some(reason)
  }

  trait BodyState {
    val symptoms: Set[Symptom]
  }

  trait ThermalBodyState extends BodyState {}

  case object SevereHeatstroke extends ThermalBodyState {
    val symptoms = Set(Exhaustion, Nauseous, Dizzy, Heart_Pounding, Convulsions)
  }
  case object HeatStroke extends ThermalBodyState {
    val symptoms = Set(Sweating, Flushed, Dizzy, Exhaustion)
  }
  case object Hot extends ThermalBodyState {
    val symptoms = Set(Sweating, Flushed)
  }
  case object Warm extends ThermalBodyState {
    val symptoms = Set(Sweating)
  }
  case object Comfortable extends ThermalBodyState {
    val symptoms: Set[Symptom] = Set.empty
  }
  case object Chilly extends ThermalBodyState {
    val symptoms = Set(Shivering)
  }
  case object Cold extends ThermalBodyState {
    val symptoms = Set(Shivering, Chattering)
  }
  case object Hypothermic extends ThermalBodyState {
    val symptoms = Set(Tingling, Numb, Exhaustion)
  }
  case object SevereHypothermic extends ThermalBodyState {
    val symptoms = Set(Numb, Sweating, Exhaustion, ColdHot)
  }

  trait BreathingBodyState extends BodyState {}
  case object HyperOxygenated extends BreathingBodyState {
    val symptoms: Set[Symptom] = Set.empty
  }
  case object Normal extends BreathingBodyState {
    val symptoms: Set[Symptom] = Set.empty
  }
  case object Hypoxic extends BreathingBodyState {
    val symptoms = Set(Dizzy, Disoriented, Nauseous)
  }

  trait Symptom {
    val description: String
    val priority: Int
  }
  case object Disoriented extends Symptom {
    val description: String = "Disoriented"
    val priority = 10
  }
  case object Comatose extends Symptom {
    val description: String = "Comatose"
    val priority = 11
  }
  case object Convulsions extends Symptom {
    val description: String = "Convulsions"
    val priority = 10
  }
  case object Nauseous extends Symptom {
    val description: String = "Nauseous"
    val priority = 9
  }
  case object Sweating extends Symptom {
    val description: String = "Sweating"
    val priority = 2
  }
  case object Flushed extends Symptom {
    val description: String = "Flushed"
    val priority = 4
  }
  case object Exhaustion extends Symptom {
    val description: String = "Exhausted"
    val priority = 7
  }
  case object Dizzy extends Symptom {
    val description: String = "Nauseous"
    val priority = 7
  }
  case object Heart_Pounding extends Symptom {
    val description: String = "High Heart Rate"
    val priority = 8
  }
  case object Shivering extends Symptom {
    val description: String = "Shivering"
    val priority = 2
  }
  case object Chattering extends Symptom {
    val description: String = "Chattering"
    val priority = 5
  }
  case object Tingling extends Symptom {
    val description: String = "Tingling"
    val priority = 8
  }
  case object Numb extends Symptom {
    val description: String = "Numb"
    val priority = 9
  }
  case object ColdHot extends Symptom {
    val description: String = "Hot?"
    val priority = 10
  }


  var walkThroughWalls = false
  var seeThroughWalls = false
  var showTempDebug = false
  var showGasDebug = false
  var showCableDebug = false
  var invulnerable = false

  var messages: Seq[(String, Int)] = Seq.empty
  def putMessage(message: String): Unit = messages :+= ((message, currentTime))

  def elapse(durationSec: Int): Unit = {
    for (_ <- 0 until durationSec) {
      items.all.foreach(sendMessage(_, Message.Tick))
      val start = System.nanoTime()
      updateAtmosphere(player.levelId)
      println(f"temperature+gas sim took ${(System.nanoTime() - start) / 1e6}%.2f ms")
      tickFluidNetworks()
      checkBodyTemp()
      internalCalories -= 1
      checkHunger()
      breathe()
      currentTime += 1
    }
  }

  def tickFluidNetworks(): Unit = {
    for ((levelId, networkLayers) <- fluidLayerConnections; (networkLayer, i) <- networkLayers.zipWithIndex) {
      val connectedClusters = networkLayer.values.toSet
      for (cluster <- connectedClusters) {
        val message = Message.TotalPressure(i, GasComposition.zero, 0)
        for ((x, y) <- cluster)
          broadcastToLocation(OnFloor(Location(levelId, (x, y))), message)
        val averagePressure = message.totalPressure / message.count
        for ((x, y) <- cluster)
          broadcastToLocation(OnFloor(Location(levelId, (x, y))), Message.AdjustPressure(i, averagePressure, 0.5f))
      }
    }
  }

  /**
    * Remove |part| from its parent, potentially also removing the parent from its parent, and so on, recursively.
    *
    * @param parents The stack of parents. |part| must be a part of the last element of |parents|.
    *                |parents(i)| must be a part of |parents(i-1)|. |parent(0)| must not be a part of any other item.
    * @param part The part to be removed.
    */
  def removePart(parents: Seq[Item], part: Item): Unit = {
    val lastParent = parents.last
    assert(lastParent.parts.contains(part))
    lastParent.parts = lastParent.parts.filter(_ ne part)
    if (!lastParent.behaviors.exists(_.isInstanceOf[MissingParts])) {
      lastParent.behaviors += MissingParts()
      sendMessage(lastParent, Message.Disassembled())
    }
    if (lastParent.parts.isEmpty) {
      if (parents.init.nonEmpty)
        removePart(parents.init, lastParent)
      else
        items.delete(lastParent)
    }
  }

  def receive(action: Action): Unit = {
    action match {
      case Action.PlayerMove(dx, dy) =>
        if (inLevel(player + (dx, dy)) && (canWalk(player + (dx, dy)) || walkThroughWalls)) {
          movePlayer(player + (dx, dy))
        } else {
          val loc = player + (dx, dy)
          broadcastToLocation(OnFloor(loc), PlayerBump(loc))
        }
        elapse(1)

      case Action.Remove(parents, item) =>
        val disassembleOp = parents.last.kind.parts.find(_.kind == item.kind).get.operation
        if (disassembleOp.id == "HANDLING" ||
          nearbyItems.exists { tool => sendMessage(tool, Message.UseTool(disassembleOp)).ok }) {
          items.put(item, OnFloor(player))
          removePart(parents, item)
          elapse(10)
          putMessage(s"You remove the ${itemDisplayName(item)} from the ${itemDisplayName(parents.last)}.")
        } else {
          putMessage(s"You need a ${disassembleOp.id} tool to do that.")
        }

      case Action.Install(parent, part) =>
        val op = parent.kind.parts.find(_.kind == part.kind).get.operation
        if (op.id == "HANDLING" ||
          nearbyItems.exists { tool => sendMessage(tool, Message.UseTool(op)).ok }) {
          items.delete(part)
          parent.parts = parent.parts :+ part
          sendMessage(parent, PartInstalled())
          elapse(10)
          putMessage(s"You install the ${itemDisplayName(part)} into the ${itemDisplayName(parent)}")
        } else {
          putMessage(s"You need a ${op.id} tool to do that.")
        }

      case Action.Diagnose(item) =>
        // TODO: check tools, elapse time, etc
        val m = sendMessage(item, Message.IsDiagnosable())
        if (m.diagnosable) {
          val diagnoseOp = m.opRequired.get
          if (diagnoseOp.id == "HANDLING" ||
            nearbyItems.exists { tool => sendMessage(tool, Message.UseTool(diagnoseOp)).ok }) {
            sendMessage(item, Message.Diagnose())
            elapse(10)
          } else {
            putMessage(s"You need a ${diagnoseOp.id} tool to do that.")
          }
        } else {
          putMessage("That can't be diagnosed.")
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
      case Action.MoveItem(item, toLocation) =>
        toLocation match {
          case Inside(container) =>
            @tailrec
            def containedWithin(a: Item, b: Item): Boolean = {
              items.lookup(a) match {
                case Inside(other) =>
                  other == b || containedWithin(other, b)
                case _ => false
              }
            }
            if (item == container || containedWithin(container, item)) {
              putMessage(s"You can't put ${itemDisplayName(item)} inside itself.")
            } else {
              val canContain = sendMessage(container, CanContain(item))
              if (!canContain.ok) {
                putMessage(s"You can't put ${itemDisplayName(item)} into the ${itemDisplayName(container)}${canContain.reason.map(r => s" ($r)").getOrElse("")}.")
              } else {
                putMessage(s"You put the ${itemDisplayName(item)} into the ${itemDisplayName(container)}.")
                items.move(item, toLocation)
              }
            }
            // TODO: handle other cases better
          case _ =>
            items.move(item, toLocation)
        }
        elapse(5)

      case Action.Wear(item) =>
        if (!sendMessage(item, Message.CanWear()).ok)
          putMessage(s"You can't wear that.")
        else {
          items.move(item, Worn())
          elapse(5)
          putMessage(s"You put on the ${itemDisplayName(item)}.")
        }

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
    if (bodyTemp > 41 + K && bodyTemp <= 42 + K) {
      if (random.oneIn(10)) {
        thermalBodyState = SevereHeatstroke
      }
      if (random.oneIn(60)) {
        die(reason="heatstroke")
      }
    }

    if (bodyTemp > 40 + K && bodyTemp <= 41 + K) {
      if (random.oneIn(10)) {
        thermalBodyState = HeatStroke
      }
    }

    if (bodyTemp > 39 + K && bodyTemp <= 40 + K) {
      if (random.oneIn(10)) {
        thermalBodyState = Hot
      }
    }

    if (bodyTemp > 38 + K && bodyTemp <= 39 + K) {
      if (random.oneIn(10)) {
        thermalBodyState = Warm
      }
    }

    if (bodyTemp > 36 + K && bodyTemp <= 38 + K) {
      if (random.oneIn(10)) {
        thermalBodyState = Comfortable
      }
    }
    if (bodyTemp > 35 + K && bodyTemp <= 36 + K) {
      if (random.oneIn(60)) {
        putMessage("You shiver.")
        thermalBodyState = Chilly
      }
    } else if (bodyTemp > 34 + K && bodyTemp <= 35 + K) {
      if (random.oneIn(30)) {
        putMessage("Your teeth chatter. It's freezing cold.")
        thermalBodyState = Cold
      }
    } else if (bodyTemp > 33 + K && bodyTemp <= 34 + K) {
      if (random.oneIn(10)) {
        putMessage("You're so cold you can't even shiver.")
        thermalBodyState = Hypothermic
      }
    } else if (bodyTemp <= 33 + K) {
      if (random.oneIn(10)) {
        putMessage("Is it hot in here? No, that can't be right...")
        thermalBodyState = SevereHypothermic
      }
      if (random.oneIn(60)) {
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

  private def breathe(): Unit = {
    val gas = levels(player.levelId).gasComposition(player.xy)
    if (gas.oxygen < 1) {
      if (random.oneIn(10)) {
        putMessage("You black out.")
        die("hypoxia")
      } else if (random.oneIn(5)) {
        putMessage(random.oneOf(
          "Your head pounds. Is that a light?",
          "You can hardly draw breath.",
          "You stumble.",
          "Wait... where are you?",
        ))
      }
    } else if (gas.oxygen < 5) {
      if (random.oneIn(30)) {
        putMessage(random.oneOf(
          "You feel light-headed.",
          "Your fingers tingle.",
          "Your toes feel numb.",
          "You feel tired.",
        ))
      }
    } else if (gas.oxygen < 10) {
      if (random.oneIn(60)) {
        putMessage(random.oneOf(
          "Your head aches.",
          "It feels like an effort to lift your limbs.",
        ))
      }
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

  def toolsProviding(op: ItemOperation): Seq[Item] = nearbyItems.filter(i => sendMessage(i, Message.Provides(op)).provides)

  def visibleConditions(item: Item): Seq[String] =
    sendMessage(item, Message.VisibleConditions(sendMessage(item, Message.Conditions()).conditions)).conditions

  def itemDisplayName(item: Item): String = {
    var name = item.kind.name
    val conditions = visibleConditions(item)
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

  def sampleItem2[T](table: Population.Table[T], f: ItemWithExtras => T, getMeTheActualNameOfTheItem: T => ItemWithExtras): Seq[(Item, T)] = {
    for {
      t <- table.sample()(random, data.itemGroups.view.mapValues(_.choose).mapValues(_.map(f)))
    } yield {
      val itemKindName = getMeTheActualNameOfTheItem(t)
      data.items.get(itemKindName.item) match {
        case Some(itemKind) =>
          (itemKind.generateItem(), t)
        case None =>
          throw new RuntimeException(s"""No item with name "$itemKindName"""")
      }
    }
  }

  def sampleItemOnly(table: Population.Table[String]): Seq[Item] =
    sampleItem(table.map(ItemWithExtras(_)))

  def sampleItem(table: Population.Table[ItemWithExtras]): Seq[Item] = {
    sampleItemWithExtras(table) map { itemWithExtras =>
      data.items.get(itemWithExtras.item) match {
        case Some(itemKind) =>
          val item = itemKind.generateItem()
          for (containedItem <- sampleItem(itemWithExtras.contents)) {
            items.put(containedItem, Inside(item))
          }
          item
        case None =>
          throw new RuntimeException(s"""No item with name "${itemWithExtras.item}"""")
      }
    }
  }

  def sampleItemWithExtras(table: Population.Table[ItemWithExtras]): Seq[ItemWithExtras] =
    table.sample()(random, data.itemGroups.view.mapValues(_.choose))

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

  def isKnownToBeNonFunctional(p: Item): Boolean =
    visibleConditions(p).nonEmpty || p.parts.exists(isKnownToBeNonFunctional)

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

  def volume(item: Item): Volume =
    item.kind.volume + sendMessage(item, Message.ExtraVolume()).volume

  def terrain(l: Location): Option[Terrain] = levels(l.levelId).terrain.get(l.x, l.y)

  def inLevel(l: Location): Boolean = terrain(l).nonEmpty
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
    var needsUpdate = Set.empty[LevelId]
    while (dirtyPermeabilityTiles.nonEmpty) {
      val l = dirtyPermeabilityTiles.dequeue()
      isPermeableCache(l.levelId)(l.x, l.y) = isPermeable(l)
      needsUpdate += l.levelId
    }
    for (levelId <- needsUpdate) {
      val permeability = isPermeableCache(levelId)
      levels(levelId).updateAtmoSimStatics((x: Int, y: Int) => permeability(x, y))
    }
  }

  def refresh(): Unit = {
    recomputePermeabilityCache()
    recalculateFOV()
    for ((levelId, level) <- levels) {
      val permeability = isPermeableCache(levelId)
      level.updateAtmoSimStatics((x: Int, y: Int) => permeability(x, y))
    }
    println("method 1")
    for (i <- 1 to 10) {
      val begin = System.nanoTime()
      recalculateConnections()
      println(f"  recalculating connections took ${(System.nanoTime() - begin) / 1e6}%.2f ms")
    }
    println("method 2")
    for (i <- 1 to 10) {
      val begin = System.nanoTime()
      recalculateConnections2()
      println(f"  recalculating connections took ${(System.nanoTime() - begin) / 1e6}%.2f ms")
    }
  }

  def isPermeable(l: Location): Boolean = {
    val m = Message.IsPermeable(terrain(l).exists(_.permeable))
    items.lookup(OnFloor(l)).foreach(i => sendMessage(i, m))
    m.permeable
  }

  def isEdible(item: Item): Boolean = sendMessage(item, Message.IsEdible()).edible
  def eat(item: Item): Unit =
    internalCalories += sendMessage(item, Message.Eat()).calories


  // items can be _diagnosable_ with a tool type, or automatically diagnosed (i.e. no tool required to know if broken).
  // both composite and atomic items can be diagnosable.
  // if an item is diagnosable, that means its broken/not-broken state is not obvious.
  // the brokenness state of non-diagnosable items is always visible.
  // for diagnosable items, to test whether the item is functional, a diagnose action and a tool is required.
  // -? does the diagnosis last forever? what if the item is re-broken? does it revert to 'undiagnosed'? doesn't that
  //    just make it obvious that it's the broken part? does breaking a part of an item reset all its siblings to
  //    undiagnosed? what about the parent?
  // -> diagnosing will tag a _problem_ but it won't tag the _lack of a problem_.
  //

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

  var powerLayerConnections: Map[LevelId, Array[Map[(Int, Int), Set[(Int, Int)]]]] = Map.empty
  var dataLayerConnections: Map[LevelId, Array[Map[(Int, Int), Set[(Int, Int)]]]] = Map.empty
  var fluidLayerConnections: Map[LevelId, Array[Map[(Int, Int), Set[(Int, Int)]]]] = Map.empty
  def recalculateConnections(): Unit = {
    def rc(getCables: Level => Grid[Int]): Map[LevelId, Array[Map[(Int, Int), Set[(Int, Int)]]]] = {
      val newCableLayerConnections = mutable.Map.empty[LevelId, Array[Map[(Int, Int), Set[(Int, Int)]]]]
      for ((levelId, level) <- levels) {
        newCableLayerConnections(levelId) = new Array(8)
        val hasCableOnLayer = Array.fill(8)(mutable.Set.empty[(Int, Int)])
        // Find all the locations with cables.
        for (xy <- level.powerCables.indices) {
          val layers = getCables(level)(xy)
          if (layers != 0) {
            for (i <- 0 until 8) {
              val mask = 1 << i
              if (layers != 0) {
                if ((layers & mask) != 0) {
                  hasCableOnLayer(i).add(xy)
                }
              }
            }
          }
        }

        def adj4(xy: (Int, Int)): Seq[(Int, Int)] = Seq(
          (xy._1 - 1, xy._2),
          (xy._1, xy._2 - 1),
          (xy._1 + 1, xy._2),
          (xy._1, xy._2 + 1),
        )

        for (i <- 0 until 8) {
          // Identify all the connected components.
          val components = mutable.Set.empty[Set[(Int, Int)]]
          val cellToComponent = mutable.Map.empty[(Int, Int), Set[(Int, Int)]]
          while (hasCableOnLayer(i).nonEmpty) {
            val first = hasCableOnLayer(i).head
            val reachable = BFS.reachableFrom[(Int, Int)](first, xy => adj4(xy).filter(hasCableOnLayer(i)))
            hasCableOnLayer(i) --= reachable
            for (cell <- reachable)
              cellToComponent(cell) = reachable
            components += reachable
          }
          newCableLayerConnections(levelId)(i) = cellToComponent.toMap
        }
      }
      newCableLayerConnections.toMap
    }
    powerLayerConnections = rc(_.powerCables)
    dataLayerConnections = rc(_.dataCables)
    fluidLayerConnections = rc(_.fluidCables)
  }

  def recalculateConnections2(): Unit = {
    def rc(getCables: Level => Grid[Int]): Map[LevelId, Array[DisjointSet[(Int, Int)]]] = {
      val newCableLayerConnections = mutable.Map.empty[LevelId, Array[DisjointSet[(Int, Int)]]]
      for ((levelId, level) <- levels) {
        val djs = Array.fill(8)(new DisjointSet[(Int, Int)])
        newCableLayerConnections(levelId) = djs
        for (xy <- getCables(level).indices) {
          val layers = getCables(level)(xy)
          if (layers != 0) {
            for (i <- 0 until 8) {
              val s = djs(i)
              val mask = 1 << i
              // Find all the locations with cables.
              if ((layers & mask) != 0) {
                s.makeSet(xy)
                s.union(xy, (xy._1 - 1, xy._2))
                s.union(xy, (xy._1, xy._2 - 1))
              }
            }
          }
        }
      }
      newCableLayerConnections.toMap
    }
    //powerLayerConnections = rc(_.powerCables)
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

  def updateAtmosphere(levelId: LevelId, dt: Float = 1): Unit = {
    val level = levels(levelId)
    processPermeabilityUpdateQueue()
    level.updateAtmosphere(dt)(random)

    // transfer heat between player & environment
    val playerHeatCapacity = 4 // ~water

    val playerTileTemp = level.temperature(player.x, player.y)
    val k = 0.01f
    val w = (bodyTemp - playerTileTemp) * k

    val dq = broadcastToLocation(Worn(), Message.LoseHeat(dq = w * dt / 20)).dq
    bodyTemp -= dq / playerHeatCapacity
    level.setTemperature(player.x, player.y, level.temperature(player.x, player.y) + dq / level.terrain(player.x, player.y).heatCapacity)

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
    val maxMetabolismDq = 0.01f
    val metabolismDq = math.max(0, math.min((310 - bodyTemp) * 0.9f, maxMetabolismDq))
    bodyTemp += metabolismDq / playerHeatCapacity

    // Respiration
    // https://www3.nd.edu/~nsl/Lectures/mphysics/Medical%20Physics/Part%20I.%20Physics%20of%20the%20Body/Chapter%203.%20Pressure%20System%20of%20the%20Body/3.1%20Physics%20of%20breathing/Physics%20of%20breathing.pdf
    // We breathe about 500 ml of air per breath (Tidal Volume)
    // When inhaled air is ~20% O2, exhaled air is ~15% O2.
    // The volume of gas exchanged is between 0.3 L/min and 1.0 L/min depending on activity level.
    // i.e. 0.3 L O2 removed, 0.3 L CO2 added.
    // PV = nRT
    // we know P, V, T, R = 8.31 J/K/mol
    // so n = RT/PV

    // breathing rate ~ 8 g / min = 1.6 x 10^23 molecules / min (all gases)

    // 44.5 moles of atmosphere in 1000 L (STP)     21% O2 = 9 moles of O2
    // 1.6e23 molecules / min = 2.657 moles exchanged per min

    // somewhere in the range of 0.01 ~ 0.07 kPa O2 exchanged per second?


    // Or: humans use about 500 L of O2 per day at STP.
    // in 1000 L of 21% O2 at STP we have 210 L of O2.
    // So if you're in a single enclosed cell, 210 L will last you 10 hours.
    // => you'll use up 21 kPa of O2 in 10 hrs
    // => 2.1 kPa in 1 hr
    // => 0.0006 kPa in 1 sec

    val playerGC = level.gasComposition(player.xy)
    val respirationRate = 0.0006f
    val respirated = math.min(playerGC.oxygen, respirationRate * dt)
    level.setGasComposition(player.x, player.y, playerGC + GasComposition(oxygen = -respirated, carbonDioxide = respirated, nitrogen = 0))
  }
}
