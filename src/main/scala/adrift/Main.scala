package adrift

import java.nio.file.Paths

import adrift.display.{Display, GLFWDisplay}
import adrift.worldgen.WorldGen

object Main {
  def main(args: Array[String]): Unit = {
    val data = Data.parse(Paths.get("data"))
    val state = WorldGen(data).generateWorld
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
