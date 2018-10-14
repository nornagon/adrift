package adrift.worldgen

import adrift.worldgen.WaveFunctionCollapse.GraphTileSet
import adrift.{Furniture, GameState, Terrain}

import scala.util.Random

object WorldGen {
  sealed trait ConnectionType {
    def rotated: ConnectionType = this
  }
  case object Open extends ConnectionType
  case object Wall extends ConnectionType
  case object Door extends ConnectionType
  case class Internal(i: Int, r: Int = 0) extends ConnectionType {
    override def rotated: Internal = copy(r = (r + 1) % 4)
  }

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
      left = up.rotated,
      up = right.rotated,
      right = down.rotated,
      down = left.rotated,
      rotation = (rotation + 1) % 4
    )
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
  )
  val corridorT = Room(
    "corridor",
    left = Wall,
    right = Open,
    up = Open,
    down = Open,
    rotatable = true
  )

  val labLeft = Room(
    "labLeft",
    left = Wall,
    right = Internal(1),
    up = Wall,
    down = Door,
    rotatable = true
  )
  val labRight = Room(
    "labRight",
    left = Internal(1),
    right = Wall,
    up = Wall,
    down = Wall,
    rotatable = true
  )

  val farmLeft = Room(
    "farmLeft",
    left = Wall,
    right = Internal(2),
    up = Internal(3),
    down = Door,
    rotatable = true
  )
  val farmRight = Room(
    "farmRight",
    left = Internal(2),
    right = Wall,
    up = Internal(4),
    down = Wall,
    rotatable = true
  )
  val farmTopLeft = Room(
    "farmTop",
    left = Wall,
    right = Internal(5),
    up = Wall,
    down = Internal(3),
    rotatable = true
  )
  val farmTopRight = Room(
    "farmRight",
    left = Internal(5),
    right = Wall,
    up = Wall,
    down = Internal(4),
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
    val tiles = new RoomTiles(Seq(corridor, corridorWithDoor, corridorEnd, corridorX, corridorT,
      quarters,
      labLeft, labRight,
      farmLeft, farmRight, farmTopLeft, farmTopRight,
    ))
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
          state.furniture(x + 3, y) = Some(Furniture.AutomaticDoor())
        case Open | Internal(_, _) =>
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
          state.furniture(x, y + 3) = Some(Furniture.AutomaticDoor())
        case Open | Internal(_, _) =>
          for (dy <- 1 to 5) {
            state.map(x, y + dy) = Terrain.Floor
          }
      }
      room.name match {
        case "labLeft" =>
          state.furniture(x + 1, y + 1) = Some(Furniture.Desk)
        case _ =>
      }
      // center
      for (dx <- 1 to 5; dy <- 1 to 5) state.map(x + dx, y + dy) = Terrain.Floor
    }
    state.movePlayer(width*3 + 3, height*3 + 3)
    state
  }
}
