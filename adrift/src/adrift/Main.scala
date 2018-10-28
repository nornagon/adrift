package adrift

import java.nio.file.Paths

import adrift.display.{Display, GLFWDisplay}
import adrift.worldgen.WorldGen

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val data = Data.parse(Paths.get("data"))
    implicit val random: Random = new Random(52)
    val state = WorldGen(data).generateWorld
    state.recalculateFOV()
    val display: Display = new GLFWDisplay
    display.init()
    display.update(state)
    while (display.running) {
      val action = display.waitForAction
      state.receive(action)
      display.update(state)
    }
  }
}
