package adrift

import adrift.Action._

class GameState {
  val map: Grid[Int] = new Grid[Int](80, 24)(46)
  var player: (Int, Int) = (19, 9)
  def receive(action: Action): Unit = {
    action match {
      case PlayerMove(dx, dy) =>
        player = (player._1 + dx, player._2 + dy)
      case Quit =>
    }
  }
}
