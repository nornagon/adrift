package adrift

import adrift.display.{Display, GLFWDisplay}

object Main {
  def main(args: Array[String]): Unit = {
    val state = GameState.generateWorld2
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
