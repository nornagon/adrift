package adrift.worldgen

import adrift.RandomImplicits._
import adrift.YamlObject.ItemGroup
import adrift._
import adrift.items.Item
import adrift.items.behaviors.Broken

import scala.util.Random

case class WorldGen(data: Data)(implicit random: Random) {
  def clamp01(d: Double) = math.max(0, math.min(1, d))
  def generateWorld: GameState = {
    val layout = BSPArchitect.generate()
    val state = new GameState(data, new Random(random.nextLong()))
    val (width, height) = (layout.bounds.width + 1, layout.bounds.height + 1)

    val levelId = LevelId("main")
    val level = Level(
      terrain = new CylinderGrid(width, height)(data.terrain("floor")),
      temperature = new CylinderGrid(width, height)(random.between(250d, 270d)),
      gasComposition = new CylinderGrid(width, height)(GasComposition(4, 9, 1))
    )
    state.levels(levelId) = level

    val isCorridor = new CylinderGrid(width, height)(true)

    for (room <- layout.rooms) {
      for (y <- room.t to room.b; x <- room.l to room.r) isCorridor(x, y) = false

      for (y <- room.t to room.b) {
        level.terrain(room.l, y) = data.terrain("wall")
        level.terrain(room.r, y) = data.terrain("wall")
      }
      for (x <- room.l to room.r) {
        level.terrain(x, room.t) = data.terrain("wall")
        level.terrain(x, room.b) = data.terrain("wall")
      }
    }

    for (room <- layout.rooms) {
      val canGenerateDoorOnLeft = isCorridor.get(room.l - 1, room.t).contains(true)
      val canGenerateDoorOnRight = isCorridor.get(room.r + 1, room.t).contains(true)
      val canGenerateDoorOnTop = isCorridor.get(room.l, room.t - 1).contains(true)
      val canGenerateDoorOnBottom = isCorridor.get(room.l, room.b + 1).contains(true)
      val possibleDoorPositions: Seq[(Int, Int)] = {
        (if (canGenerateDoorOnLeft) {
          val doorPosition = (clamp01(0.5 + random.nextGaussian() * 0.2) * math.max(0, room.b - room.t - 2)).round.toInt + room.t + 1
          Seq((room.l, doorPosition))
        } else Seq.empty) ++ (if (canGenerateDoorOnRight) {
          val doorPosition = (clamp01(0.5 + random.nextGaussian() * 0.2) * math.max(0, room.b - room.t - 2)).round.toInt + room.t + 1
          Seq((room.r, doorPosition))
        } else Seq.empty) ++ (if (canGenerateDoorOnTop) {
          val doorPosition = (clamp01(0.5 + random.nextGaussian() * 0.2) * math.max(0, room.r - room.l - 2)).round.toInt + room.l + 1
          Seq((doorPosition, room.t))
        } else Seq.empty) ++ (if (canGenerateDoorOnBottom) {
          val doorPosition = (clamp01(0.5 + random.nextGaussian() * 0.2) * math.max(0, room.r - room.l - 2)).round.toInt + room.l + 1
          Seq((doorPosition, room.b))
        } else Seq.empty)
      }
      if (possibleDoorPositions.nonEmpty) {
        val (x, y) = random.pick(possibleDoorPositions)
        level.terrain(x, y) = data.terrain("floor")
        generateItem(data.itemGroups("automatic door")).foreach(state.items.put(_, OnFloor(Location(levelId, x, y))))
      }
    }

    val roomTypes = Seq("quarters", "lounge", "medical", "cryopods")

    for (room <- layout.rooms) {
      if (room.width > 2 && room.height > 2) {
        val cells: Seq[(Int, Int)] = for (x <- room.l + 1 to room.r - 1; y <- room.t + 1 to room.b - 1) yield (x, y)
        data.roomgens(random.pick(roomTypes)).generate(state, levelId, cells)
      } else {
        println(s"Warning: tiny room: ${room.width} x ${room.height}")
      }
    }

    state
  }

  def generateItem(itemGroup: ItemGroup): Seq[Item] = {
    itemGroup.choose.sample()(random, data.itemGroups.view.mapValues(_.choose)).map { itemId =>
      data.items(itemId).generateItem()
    }
  }

  def break(item: Item): Unit = {
    if (item.parts.isEmpty) {
      println(s"Breaking ${item.kind.name}")
      item.behaviors.append(Broken())
    } else {
      break(random.pick(item.parts))
    }
  }

  def damage(state: GameState): Unit = {
    for (item <- state.items.all) {
      if (item.kind.name == "automatic door") {
        if (random.oneIn(10)) {
          println(s"Breaking door at ${state.items.lookup(item)}")
          break(item)
        }
      }
    }
  }
}
