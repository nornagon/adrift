package adrift

import java.nio.file.Paths

import adrift.display.{Display, GLFWDisplay, GLFWWindow}
import adrift.worldgen.{RoomGen, WFC}
import adrift.RandomImplicits._

import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success, Try}

object RoomGenSizeTest {
  def main(args: Array[String]): Unit = {
    val dataPath = Paths.get("data")
    val data = Data.parse(dataPath)
    val roomGen = data.roomgens(args(0))
    for (width <- 5 to 20; height <- 5 to 20) {
      val state = new GameState(data, new Random(42))
      val levelId = LevelId("main")
      val level = Level.emptySquare(data, width, height)(new Random(42))
      state.levels(levelId) = level
      for (y <- 0 until height) {
        level.terrain(0, y) = data.terrain("wall")
        level.terrain(width - 1, y) = data.terrain("wall")
      }
      for (x <- 0 until width) {
        level.terrain(x, 0) = data.terrain("wall")
        level.terrain(x, height - 1) = data.terrain("wall")
      }
      val cells = for (y <- 1 until height - 1; x <- 1 until width - 1) yield (x, y)
      Try {
        roomGen.algorithm.generate(state, levelId, cells)(new Random(42))
      } match {
        case Success(value) =>
          println(s"Successfully generated for size $width x $height")
        case Failure(exception) =>
          println(s"Couldn't generate for size $width x $height")
      }
    }
  }
}

object RoomGenTest {
  def main(args: Array[String]): Unit = {
    val dataPath = Paths.get("data")
    val window = new GLFWWindow
    val display: Display = new GLFWDisplay(window, Main.font)
    display.init()
    window.show()
    window.poll()
    val forcedWidth = if (args.length > 1) Some(args(1).toInt) else None
    val forcedHeight = if (args.length > 2) Some(args(2).toInt) else None
    val random = new Random

    val initialData = Data.parse(dataPath)

    var state: GameState = null
    def data = if (state == null) initialData else state.data
    def regenerate(seed: Long): Unit = {
      implicit val random = new Random(seed)
      val width = forcedWidth.getOrElse(math.max(3, math.min(15, 10 + random.nextGaussian() * 5)).round.toInt)
      val height = forcedHeight.getOrElse(math.max(3, math.min(15, 10 + random.nextGaussian() * 5)).round.toInt)
      state = generate(data, data.roomgens(args.head), width, height, window)
      state.refresh()
      state.recalculateFOV()
    }
    Iterator.continually(Try { regenerate(random.nextLong) }).take(5).find(_.isSuccess) getOrElse {
      throw new RuntimeException(s"Couldn't generate a room")
    }

    display.update(state)
    FileWatcher.onFileChanged(dataPath) { _ =>
      display.postAction(Action.ReloadData(Data.parse(dataPath)))
    }

    while (display.running) {
      val action = display.waitForAction
      action match {
        case Action.Regenerate =>
          Iterator.continually(Try { regenerate(random.nextLong) }).take(5).find(_.isSuccess)
        case _ =>
          state.receive(action)
      }
      display.update(state)
    }
  }

  def generate(data: Data, roomgen: RoomGen, width: Int, height: Int, window: GLFWWindow)(implicit random: Random): GameState = {
    println(s"Generating $width x $height room...")
    val state = new GameState(data, random)
    val levelId = LevelId("main")
    val level = Level.emptySquare(data, width, height)
    state.levels(levelId) = level
    for (y <- 0 until height) {
      level.terrain(0, y) = data.terrain("wall")
      level.terrain(width-1, y) = data.terrain("wall")
    }
    for (x <- 0 until width) {
      level.terrain(x, 0) = data.terrain("wall")
      level.terrain(x, height-1) = data.terrain("wall")
    }
    val possibleDoorLocations = for {
      y <- 0 until height
      x <- 0 until width
      if (x == 0 || x == width - 1 || y == 0 || y == height - 1) && !(x == 0 && y == 0) && !(x == width - 1 && y == 0) && !(x == width - 1 && y == height - 1) && !(x == 0 && y == height - 1)
    } yield (x, y)
    val door = random.pick(possibleDoorLocations)
    level.terrain(door) = data.terrain("floor")
    state.sampleItem(data.itemGroups("automatic door").choose).foreach(state.items.put(_, OnFloor(Location(levelId, door._1, door._2))))
    val cells = for (y <- 1 until height - 1; x <- 1 until width - 1) yield (x, y)
    roomgen.algorithm match {
      case wfc: WFC =>
        val colors = "white yellow red brown green blue light_gray gray".split(" ").map(data.display.palette(_)).to(IndexedSeq)
        def isDoorEdge(x: Int, y: Int): Boolean = (1 + x, 1 + y) == door
        var n = 0
        val tileGrid = wfc.generateChars(width - 2, height - 2, isDoorEdge, (getTile: (Int, Int) => WFC#Tile, isContradiction: Boolean) => {
          n += 1
          try {
            if (n % 1 == 0 || isContradiction) {
              window.render { g =>
                val gr = g.glyphs(Main.font)
                val (cx, cy) = gr.bounds.center
                for (y <- 0 until height - 2; x <- 0 until width - 2) {
                  val tile = getTile(x, y)
                  if (tile != null) {
                    val fg = if (tile.partId >= 0) colors(tile.partId % colors.size) else Color.White
                    gr.drawChar(cx - (width - 2) / 2 + x, cy - (height - 2) / 2 + y, tile.value, fg = fg)
                  }
                }
                if (isContradiction) {
                  gr.drawString(0, 0, "contradiction")
                }
              }
            }
          } catch {
            case NonFatal(e) => e.printStackTrace()
          }
        })
        wfc.fill(state, levelId, 1, 1, tileGrid)
      case algo =>
        algo.generate(state, levelId, cells)
    }
    state.player = Location(levelId, width / 2, height / 2)
    state.isRoomTest = true
    state
  }
}
