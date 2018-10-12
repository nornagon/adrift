package adrift

import adrift.display.{Display, GLFWDisplay}
import adrift.worldgen.WorldGen

object Main {
  def main(args: Array[String]): Unit = {
    val state = WorldGen.generateWorld
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
