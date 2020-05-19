package adrift

import java.nio.file.Paths

import adrift.display.{Display, GLFWDisplay}
import adrift.worldgen.RoomGen

import scala.util.{Random, Try}

object RoomGenTest {
  def main(args: Array[String]): Unit = {
    val dataPath = Paths.get("data")
    val data = Data.parse(dataPath)
    val display: Display = new GLFWDisplay
    val random = new Random

    var state: GameState = null
    def regenerate(seed: Long): Unit = {
      implicit val random = new Random(seed)
      val width = math.max(3, math.min(15, 10 + random.nextGaussian() * 5)).round.toInt
      val height = math.max(3, math.min(15, 10 + random.nextGaussian() * 5)).round.toInt
      state = generate(data, data.roomgens(args.head), width, height)
      state.refresh()
      state.recalculateFOV()
    }
    regenerate(random.nextLong)

    display.init()
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

  def generate(data: Data, roomgen: RoomGen, width: Int, height: Int)(implicit random: Random): GameState = {
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
    val cells = for (y <- 1 until height - 1; x <- 1 until width - 1) yield (x, y)
    roomgen.generate(state, levelId, cells)
    state.player = Location(levelId, width / 2, height / 2)
    state.isRoomTest = true
    state
  }
}
