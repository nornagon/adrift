package adrift.worldgen

import scala.util.Random
import adrift.{GameState, Terrain}

object WorldGen {
  sealed trait ConnectionType
  case object Open extends ConnectionType
  case object Walled extends ConnectionType
  case object Door extends ConnectionType

  case class Room(
    left: ConnectionType,
    right: ConnectionType,
    up: ConnectionType,
    down: ConnectionType,
  )
  val corridor = Room(
    left = Open,
    right = Open,
    up = Walled,
    down = Walled,
  )

  def generateWorld: GameState = {
    val random = new Random(42)
    val state = new GameState(2048, 128)
    for ((x, y) <- state.map.indices) {
      state.map(x, y) = Terrain.Wall
    }
    val tiles = new WaveFunctionCollapse.TileSet {
      def size = 2
      def propagator(dir: Int, t: Int): Set[Int] = {
        return Set(1 - t)
      }
    }
    println(WaveFunctionCollapse.solve(tiles, 8, 8, random))
    // TODO: more interesting tiles :D
    state
  }
}
