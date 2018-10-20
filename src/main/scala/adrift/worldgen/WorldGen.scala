package adrift.worldgen

import adrift._
import adrift.items.{Item, ItemKind}
import adrift.worldgen.WaveFunctionCollapse.GraphTileSet

import scala.collection.mutable
import scala.util.Random

case class WorldGen(data: Data) {
  sealed trait ConnectionType {
    def rotated: ConnectionType = this
  }
  case object Open extends ConnectionType
  case object Wall extends ConnectionType
  case object Door extends ConnectionType
  case class Internal(s: String, r: Int = 0) extends ConnectionType {
    override def rotated: Internal = copy(r = (r + 1) % 4)
  }

  case class Room(
    name: String,
    left: ConnectionType,
    right: ConnectionType,
    up: ConnectionType,
    down: ConnectionType,
    rotatable: Boolean = false,
    rotation: Int = 0,
    fill: Option[(GameState, (Int, Int) => (Int, Int)) => Unit]
  ) {
    def rotated: Room = copy(
      left = up.rotated,
      up = right.rotated,
      right = down.rotated,
      down = left.rotated,
      rotation = (rotation + 1) % 4
    )
  }

  def conn(s: String): ConnectionType = s match {
    case "DOOR" => Door
    case "OPEN" => Open
    case _ => Wall
  }

  val rooms = data.rooms.values.toList.flatMap { rd =>
    val layout = rd.layout.split("\n").map(_.chars.toArray)
    assert(layout.length > 0, s"${rd.name}: empty layout")
    assert(layout.forall(_.length == layout(0).length), s"${rd.name}: mismatched layout")
    // valid room dimensions: 5, 11, 17, 23
    // i.e. v = k*6-1
    //  ==> (v+1) / 6 = k
    assert((layout.length + 1) / 6 * 6 == layout.length + 1, s"${rd.name}: non-multiple-of-5 layout")
    assert((layout(0).length + 1) / 6 * 6 == layout(0).length + 1, s"${rd.name}: non-multiple-of-5 layout")

    def doFill(s: GameState, xf: (Int, Int) => (Int, Int)): Unit = {
      for ((row, y) <- layout.zipWithIndex; (cell, x) <- row.zipWithIndex) {
        val (tx, ty) = xf(x, y)
        if (cell == '.')
          s.map(tx, ty) = Terrain.Floor
        else
          s.map(tx, ty) = Terrain.Wall
      }
    }

    for (y <- 0 until (layout.length + 1) / 6; x <- 0 until (layout(0).length + 1) / 6) yield {
      val left = rd.connections.get(s"$x,$y left").map(conn).getOrElse {
        if (x == 0) Wall
        else Internal(s"${rd.name} ${x-1},$y h")
      }
      val right = rd.connections.get(s"$x,$y right").map(conn).getOrElse {
        if (x == layout(0).length / 5 - 1) Wall
        else Internal(s"${rd.name} $x,$y h")
      }
      val up = rd.connections.get(s"$x,$y up").map(conn).getOrElse {
        if (y == 0) Wall
        else Internal(s"${rd.name} $x,${y-1} v")
      }
      val down = rd.connections.get(s"$x,$y down").map(conn).getOrElse {
        if (y == layout.length / 5 - 1) Wall
        else Internal(s"${rd.name} $x,$y v")
      }
      Room(
        name = rd.name,
        left = left,
        right = right,
        up = up,
        down = down,
        rotatable = rd.rotatable,
        fill = if (x == 0 && y == 0) Some(doFill _) else None
      )
    }
  }

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
      if (left == -1) expanded(right).left == Wall
      else if (right == -1) expanded(left).right == Wall
      else expanded(left).right == expanded(right).left

    override def allowedVertical(top: Int, bottom: Int): Boolean =
      if (top == -1) expanded(bottom).up == Wall
      else if (bottom == -1) expanded(top).down == Wall
      else expanded(top).down == expanded(bottom).up

    override def connectedHorizontal(left: Int, right: Int): Boolean =
      expanded(left).right != Wall

    override def connectedVertical(top: Int, bottom: Int): Boolean =
      expanded(top).down != Wall
  }

  def generateWorld: GameState = {
    val random = new Random(52)
    val width = 40
    val height = 40
    val state = new GameState(width * 6, height * 6)
    for ((x, y) <- state.map.indices) {
      state.map(x, y) = Terrain.Wall
    }
    val tiles = new RoomTiles(rooms)
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
      // center
    }
    for (ty <- 0 until height; tx <- 0 until width) {
      val room = ss(tx)(ty)
      for (fill <- room.fill) {
        val (ox, oy) = room.rotation match {
          case 0 => (tx*6+1, ty*6+1)
          case 1 => (tx*6+1, ty*6+5)
          case 2 => (tx*6+5, ty*6+5)
          case 3 => (tx*6+5, ty*6+1)
        }
        val xf: (Int, Int) => (Int, Int) = room.rotation match {
          case 0 => (x, y) => (ox + x, oy + y)
          case 1 => (x, y) => (ox + y, oy - x)
          case 2 => (x, y) => (ox - x, oy - y)
          case 3 => (x, y) => (ox - y, oy + x)
        }
        fill(state, xf)
      }
    }
    state.movePlayer(width*3 + 3, height*3 + 3)
    state
  }

  def generateItem(itemKind: ItemKind): Item = {
    Item(itemKind, Seq.empty, itemKind.parts.flatMap { case ((part, count), operation) => Seq.fill(count)(generateItem(part)) })
  }
}
