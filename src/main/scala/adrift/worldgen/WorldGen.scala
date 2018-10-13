package adrift.worldgen

import adrift.worldgen.WaveFunctionCollapse.GraphTileSet
import adrift.{Furniture, GameState, Terrain}

import scala.util.Random

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
    rotatable = true
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
  val corridorX = Room(
    "corridor",
    left = Open,
    right = Open,
    up = Open,
    down = Open,
    rotatable = false
  )
  val corridorT = Room(
    "corridor",
    left = Wall,
    right = Open,
    up = Open,
    down = Open,
    rotatable = true
  )

  class RoomTiles(rooms: Seq[Room]) extends GraphTileSet {
    val expanded: Seq[Room] = rooms.flatMap {
      case r if r.rotatable =>
        Stream.iterate(r, 4)(_.rotated)
      case r =>
        Seq(r)
    }
    override def size: Int = expanded.size

    def interpret(result: Seq[Seq[Int]]): Seq[Seq[Room]] = result.map(_.map(expanded))

    override def allowedHorizontal(left: Int, right: Int): Boolean =
      expanded(left).right == expanded(right).left

    override def allowedVertical(top: Int, bottom: Int): Boolean =
      expanded(top).down == expanded(bottom).up

    override def connectedHorizontal(left: Int, right: Int): Boolean =
      expanded(left).right != Wall

    override def connectedVertical(top: Int, bottom: Int): Boolean =
      expanded(top).down != Wall
  }

  def generateWorld: GameState = {
    val random = new Random(51)
    val width = 40
    val height = 40
    val state = new GameState(width * 6, height * 6)
    for ((x, y) <- state.map.indices) {
      state.map(x, y) = Terrain.Wall
    }
    val tiles = new RoomTiles(Seq(corridor, corridorWithDoor, corridorEnd, corridorX, corridorT, quarters))
    val s = WaveFunctionCollapse.graphSolve(tiles, width, height, random).map(tiles.interpret)
    val ss = s.get
    for (ty <- 0 until height; tx <- 0 until width; x = tx * 6; y = ty * 6) {
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
    }
    state.movePlayer(width*3 + 3, height*3 + 3)
    state
  }
}
