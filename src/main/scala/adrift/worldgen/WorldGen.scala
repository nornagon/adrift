package adrift.worldgen

import adrift.worldgen.WaveFunctionCollapse.TileSet

import scala.util.Random
import adrift.{Furniture, GameState, Terrain}

object WorldGen {
  sealed trait ConnectionType
  case object Open extends ConnectionType
  case object Wall extends ConnectionType
  case object Door extends ConnectionType

  case class Room(
    name: String,
    left: ConnectionType,
    right: ConnectionType,
    up: ConnectionType,
    down: ConnectionType,
    rotatable: Boolean = false,
    rotation: Int = 0
  ) {
    def rotated: Room = copy(
      left = up,
      up = right,
      right = down,
      down = left,
      rotation = (rotation + 1) % 4
    )
    def dir(i: Int): ConnectionType = i match {
      case 0 => left
      case 1 => down
      case 2 => right
      case 3 => up
    }
  }
  val corridor = Room(
    "corridor",
    left = Open,
    right = Open,
    up = Wall,
    down = Wall,
  )
  val corridorWithDoor = Room(
    "corridor",
    left = Open,
    right = Open,
    up = Door,
    down = Wall,
    rotatable = true
  )
  val corridorEnd = Room(
    "corridor",
    left = Wall,
    right = Open,
    up = Wall,
    down = Wall,
    rotatable = true
  )
  val quarters = Room(
    "quarters",
    left = Wall,
    right = Wall,
    up = Wall,
    down = Door,
    rotatable = true
  )

  class RoomTiles(rooms: Seq[Room]) extends TileSet {
    val expanded: Seq[Room] = rooms.flatMap {
      case r if r.rotatable =>
        Stream.iterate(r, 4)(_.rotated)
      case r =>
        Seq(r)
    }
    override def size: Int = expanded.size

    private val opposite = Seq(2, 3, 0, 1)
    override def propagator(dir: Int, t: Int): Set[Int] = {
      val tsDirWall = expanded(t).dir(dir)
      val oppositeDir = opposite(dir)
      expanded.zipWithIndex.collect {
        case (r, i) if r.dir(oppositeDir) == tsDirWall => i
      }(collection.breakOut)
    }

    def interpret(result: Seq[Seq[Int]]): Seq[Seq[Room]] = result.map(_.map(expanded))
  }

  def generateWorld: GameState = {
    val random = new Random(51)
    val state = new GameState(2048, 128)
    for ((x, y) <- state.map.indices) {
      state.map(x, y) = Terrain.Wall
    }
    val tiles = new RoomTiles(Seq(corridor, corridorWithDoor, corridorEnd, quarters))
    val s = WaveFunctionCollapse.solve(tiles, 8, 8, random) { (x, y) =>
      var s = tiles.expanded.indices.toSet
      if (x == 0) {
        s &= tiles.expanded.zipWithIndex.collect { case (r, i) if r.left == Wall => i }.toSet
      }
      if (x == 7) {
        s &= tiles.expanded.zipWithIndex.collect { case (r, i) if r.right == Wall => i }.toSet
      }
      if (y == 0) {
        s &= tiles.expanded.zipWithIndex.collect { case (r, i) if r.up == Wall => i }.toSet
      }
      if (y == 7) {
        s &= tiles.expanded.zipWithIndex.collect { case (r, i) if r.down == Wall => i }.toSet
      }
      Some(s)
    }.map(tiles.interpret)
    val ss = s.get
    for (ty <- 0 until 8; tx <- 0 until 8; x = tx * 6; y = ty * 6) {
      val room = ss(tx)(ty)
      // top-left corner
      state.map(x, y) = Terrain.Wall
      // top wall
      room.up match {
        case Wall =>
          for (dx <- 1 to 5) {
            state.map(x + dx, y) = Terrain.Wall
          }
        case Door =>
          for (dx <- 1 to 5) {
            state.map(x + dx, y) = Terrain.Wall
          }
          state.map(x + 3, y) = Terrain.Floor
          state.furniture(x + 3, y) = Some(Furniture.DoorClosed)
        case Open =>
          for (dx <- 1 to 5) {
            state.map(x + dx, y) = Terrain.Floor
          }
      }
      // left wall
      room.left match {
        case Wall =>
          for (dy <- 1 to 5) {
            state.map(x, y + dy) = Terrain.Wall
          }
        case Door =>
          for (dy <- 1 to 5) {
            state.map(x, y + dy) = Terrain.Wall
          }
          state.map(x, y + 3) = Terrain.Floor
          state.furniture(x, y + 3) = Some(Furniture.DoorClosed)
        case Open =>
          for (dy <- 1 to 5) {
            state.map(x, y + dy) = Terrain.Floor
          }
      }
      // center
      for (dx <- 1 to 5; dy <- 1 to 5) state.map(x + dx, y + dy) = Terrain.Floor

      if (tx < 7) assert(ss(tx)(ty).right == ss(tx + 1)(ty).left, s"H mismatch at $tx,$ty: ${ss(tx)(ty)} ${ss(tx + 1)(ty)} ${tiles.propagator(0, tiles.expanded.indexOf(ss(tx)(ty))).map(tiles.expanded)}")
      if (ty < 7) assert(ss(tx)(ty).down == ss(tx)(ty+ 1).up, s"V mismatch at $tx,$ty: ${ss(tx)(ty)} ${ss(tx)(ty + 1)} ${tiles.propagator(1, tiles.expanded.indexOf(ss(tx)(ty))).map(tiles.expanded)}")
    }
    state.movePlayer(1, 2)
    state
  }
}
