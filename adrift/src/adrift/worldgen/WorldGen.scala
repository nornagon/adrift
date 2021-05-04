package adrift.worldgen

import adrift.RandomImplicits._
import adrift.YamlObject.ItemGroup
import adrift._
import adrift.items.Item
import adrift.items.behaviors.Broken

import scala.collection.mutable
import scala.util.Random

case class WorldGen(data: Data)(implicit random: Random) {
  def generateEmptyWorld(): GameState = {
    val state = new GameState(data, new Random(random.nextLong()))
    val width = 360
    val height = 270

    val levelId = LevelId("main")
    val level = Level.emptySquare(data, width, height)
    state.levels(levelId) = level

    state.player = Location(levelId, width / 2, height / 2)
    state
  }

  def generateSingleRoomWorld(roomType: String): GameState = {
    val state = new GameState(data, new Random(random.nextLong()))
    val roomGen = data.roomgens(roomType)
    val approxArea = state.random.between(math.max(roomGen.minArea, 10), math.min(roomGen.maxArea, 800))
    println(approxArea)
    val width = state.random.between(4, approxArea / 4)
    val height = approxArea / width
    println(width, height)

    val levelId = LevelId("main")
    val level = Level.emptySquare(data, width, height)
    state.levels(levelId) = level

    state.player = Location(levelId, width / 2, height / 2)

    val cells: Seq[(Int, Int)] = for (x <- 0 until width; y <- 0 until height) yield (x, y)
    data.roomgens(roomType).algorithm.generate(state, levelId, cells)

    state
  }

  def clamp01(d: Double) = math.max(0, math.min(1, d))
  def generateWorld(reportProgress: Double => Unit = (d: Double) => {}): GameState = {
    val layout = BSPArchitect.generate(360, 270)
    val state = new GameState(data, new Random(random.nextLong()))
    val (width, height) = (layout.bounds.width + 1, layout.bounds.height + 1)

    val levelId = LevelId("main")
    val level = Level.emptyCylinder(data, width, height)
    state.levels(levelId) = level

    for (x <- 0 until width) {
      // 19 is the only prime factor of 361.
      val terrain = data.terrain(if (x % 19 == 0) "transparent polycarbonate window" else "wall")
      level.terrain(x, 0) = terrain
      level.terrain(x, height - 1) = terrain
    }

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
        val doorType = random.oneOf("automatic door", "manually operated door")
        generateItem(data.itemGroups(doorType)).foreach(state.items.put(_, OnFloor(Location(levelId, x, y))))
      }
    }

    // for now, just scatter the rooms around the place
    val roomTypes = data.roomgens.keys

    val roomTypeAssignments = mutable.Buffer.empty[(Rect, String)]

    for ((room, i) <- layout.rooms.zipWithIndex) {
      val matchingRoomTypes = roomTypes.filter { t =>
        room.area >= data.roomgens(t).minArea && room.area <= data.roomgens(t).maxArea
      }
      if (room.width > 2 && room.height > 2) {
        val cells: Seq[(Int, Int)] = for (x <- room.l + 1 to room.r - 1; y <- room.t + 1 to room.b - 1) yield (x, y)
        val roomType = random.pick(matchingRoomTypes)
        try {
          data.roomgens(roomType).algorithm.generate(state, levelId, cells)
          roomTypeAssignments += ((room, roomType))
        } catch {
          case e: Throwable =>
            println(s"Error generating $roomType of size ${room.width} x ${room.height}")
            e.printStackTrace()
        }
      } else {
        println(s"Warning: tiny room: ${room.width} x ${room.height}")
      }
      reportProgress((i + 1).toDouble / layout.rooms.size)
    }

    val playerStartRoom = random.pick(roomTypeAssignments.filter(_._2 == "cryopods"))._1
    state.player = Location(levelId, playerStartRoom.center)

    damage(state)

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
