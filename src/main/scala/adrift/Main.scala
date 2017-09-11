package adrift

import adrift.display.{Display, GLFWDisplay}

object Main {
  def main(args: Array[String]): Unit = {
    val state = new GameState
    val random = new scala.util.Random(42)
    for ((x, y) <- state.map.indices) {
      if (random.nextFloat() < 0.1) state.map(x, y) = 44
      if (random.nextFloat() < 0.05) state.map(x, y) = 65
    }
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
