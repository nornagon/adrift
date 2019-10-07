package adrift.display

import adrift.{Action, GameState}

trait Display {
  def init(): Unit

  def update(state: GameState): Unit
  def waitForAction: Action
  def running: Boolean

  def postAction(action: Action): Unit
}

