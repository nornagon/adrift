package adrift

import adrift.Action._

sealed trait Terrain {
  def walkable: Boolean = true
}
case object Empty extends Terrain {
  override val walkable = false
}
case object Floor extends Terrain
case object GlassWall extends Terrain {
  override val walkable = false
}
case object TreeOak extends Terrain {
  override val walkable = false
}
case object TreeFicus extends Terrain {
  override val walkable = false
}
case object Grass extends Terrain

class GameState(width: Int, height: Int) {
  val map: Grid[Terrain] = new Grid[Terrain](width, height)(Empty)
  var player: (Int, Int) = (0, 0)
  def receive(action: Action): Unit = {
    action match {
      case PlayerMove(dx, dy) =>
        if (map(player._1 + dx, player._2 + dy).walkable)
          player = (player._1 + dx, player._2 + dy)
      case Quit =>
    }
  }
}

object GameState {
  def generateWorld: GameState = {
    val state = new GameState(128, 128)
    val random = new scala.util.Random(42)
    filledCircle(64, 64, 50) { (x, y) => state.map(x, y) = if (random.nextFloat() < 0.1) Grass else Floor }
    drawCircle(64, 64, 50) { (x, y) => state.map(x, y) = GlassWall }
    for ((x, y) <- state.map.indices) {
      if (state.map(x, y) == Grass && random.nextFloat() < 1/6f)
        state.map(x, y) = if (random.nextFloat() < 0.3) TreeFicus else TreeOak
    }
    state.player = (64, 32)
    state
  }


  def drawCircle(x0: Int, y0: Int, r: Int)(putpixel: (Int, Int) => Unit): Unit = {
    var x: Int = r-1
    var y: Int = 0
    var dx: Int = 1
    var dy: Int = 1
    var err: Int = dx - (r << 1)
    while (x >= y) {
      putpixel(x0 + x, y0 + y)
      putpixel(x0 + y, y0 + x)
      putpixel(x0 - y, y0 + x)
      putpixel(x0 - x, y0 + y)
      putpixel(x0 - x, y0 - y)
      putpixel(x0 - y, y0 - x)
      putpixel(x0 + y, y0 - x)
      putpixel(x0 + x, y0 - y)

      if (err <= 0) {
        y += 1
        err += dy
        dy += 2
      }
      if (err > 0) {
        x -= 1
        dx += 2
        err += (-r << 1) + dx
      }
    }
  }
  def filledCircle(x0: Int, y0: Int, r: Int)(putpixel: (Int, Int) => Unit): Unit = {
    var x: Int = r-1
    var y: Int = 0
    var dx: Int = 1
    var dy: Int = 1
    var err: Int = dx - (r << 1)
    var lastY = -1
    while (x >= y) {
      if (y != lastY) {
        for (xi <- x0 - x to x0 + x) putpixel(xi, y0 + y)
        for (xi <- x0 - y to x0 + y) putpixel(xi, y0 + x)
        for (xi <- x0 - x to x0 + x) putpixel(xi, y0 - y)
        for (xi <- x0 - y to x0 + y) putpixel(xi, y0 - x)
      }
      lastY = y

      if (err <= 0) {
        y += 1
        err += dy
        dy += 2
      }
      if (err > 0) {
        x -= 1
        dx += 2
        err += (-r << 1) + dx
      }
    }
  }
}