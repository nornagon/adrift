package adrift

import java.nio.file._

import adrift.display.{Display, GLFWDisplay}
import adrift.worldgen.WorldGen

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val load = args.contains("--load")

    val dataPath = Paths.get("data")
    val data = Data.parse(dataPath)

    val savePath = Paths.get("save.bson")
    val state =
      if (Files.exists(savePath) && load) {
        val start = System.nanoTime()
        val json = Bson.decode(Files.newInputStream(savePath))
        println(f"Parse took ${(System.nanoTime() - start) / 1e6}%.1f ms")
        val start2 = System.nanoTime()
        val state = Serialization.load(data, json)
        println(f"Load took ${(System.nanoTime() - start2) / 1e6}%.1f ms")
        state
      } else {
        implicit val random: Random = new Random(12367)
        val gen = WorldGen(data)
        val state = gen.generateWorld
        state.refresh()
        //val start = System.nanoTime()
        //Bson.encode(Serialization.save(state), Files.newOutputStream(savePath))
        //println(f"Save took ${(System.nanoTime() - start) / 1e6}%.1f ms")
        state
      }

    state.recalculateFOV()
    val display: Display = new GLFWDisplay
    display.init()
    display.update(state)

    FileWatcher.onFileChanged(dataPath) { _ =>
      display.postAction(Action.ReloadData(Data.parse(dataPath)))
    }

    while (display.running) {
      val action = display.waitForAction
      state.receive(action)
      display.update(state)
    }
  }
}
