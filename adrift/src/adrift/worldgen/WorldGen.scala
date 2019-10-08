package adrift.worldgen

import adrift.Population.Table
import adrift.RandomImplicits._
import adrift.YamlObject.ItemGroup
import adrift._
import adrift.items.Item
import adrift.worldgen.WaveFunctionCollapse.GraphTileSet

import scala.collection.mutable
import scala.util.Random

case class WorldGen(data: Data)(implicit random: Random) {
  sealed trait ConnectionType {
    def rotated: ConnectionType = this
    def isConnected: Boolean = true
  }
  case object Open extends ConnectionType
  case object Wall extends ConnectionType { override def isConnected: Boolean = false }
  case object Door extends ConnectionType
  case object Space extends ConnectionType { override def isConnected: Boolean = false }
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
    fill: Option[(GameState, (Int, Int) => (Int, Int)) => Unit],
  ) {
    def rotated: Room = copy(
      left = up.rotated,
      up = right.rotated,
      right = down.rotated,
      down = left.rotated,
      rotation = (rotation + 1) % 4
    )

    override def toString: String = s"Room($name, left=$left, right=$right, up=$up, down=$down)"
  }

  def conn(s: String): ConnectionType = s match {
    case "DOOR" => Door
    case "OPEN" => Open
    case "WALL" => Wall
    case "SPACE" => Space
  }

  val rooms = data.rooms.values.toList.flatMap { rd =>
    val layout = rd.layout.split("\n")
    assert(layout.nonEmpty, s"${rd.name}: empty layout")
    assert((layout.length + 1) / 6 * 6 == layout.length + 1, s"${rd.name}: rooms are 5 tiles + 1 tile for the wall")
    val tilesX = (layout.map(_.length).max + 1) / 6
    val tilesY = (layout.length + 1) / 6

    // set of x,y tiles that are present in this layout. each coordinate in this set is a 5x5 "room"
    //
    // really need better terminology to distinguish between map tiles (1x1 character), room tiles (5x5 chunks) and-
    // rooms (collections of 5x5 chunks)
    val tiles = mutable.Set.empty[(Int, Int)]
    def getLayout(rx: Int, ry: Int): Char = {
      val line = layout(ry*6)
      if (rx*6 >= line.length) ' ' else line(rx*6)
    }
    for (ry <- 0 until tilesY; rx <- 0 until tilesX) {
      val isDefined = !getLayout(rx, ry).isSpaceChar
      if (isDefined) tiles.add((rx, ry))
    }

    def doFill(x0: Int, y0: Int)(s: GameState, xf: (Int, Int) => (Int, Int)): Unit = {
      for ((row, y) <- layout.zipWithIndex; (cell, x) <- row.zipWithIndex; if !cell.isSpaceChar) {
        val (tx, ty) = xf(x - x0*6, y - y0*6)
        val cellChar = cell.toString
        val d = rd.defs.getOrElse(cellChar, throw new RuntimeException(s"Character '$cellChar' not in defs of room ${rd.name}"))
        val terrain = d("terrain").flatMap(_.asString).getOrElse(rd.default_terrain)
        s.terrain(tx, ty) = data.terrain(terrain)
        val itemTable = d("items").map(_.as[Table[String]].getOrElse(throw new RuntimeException(s"Failed to parse items")))
        itemTable.foreach { table =>
          val items = table.sample()(random, data.itemGroups.mapValues(_.choose))
          items.foreach { item_kind_id =>
            val itemKind = data.items(item_kind_id)
            s.items.put(itemKind.generateItem(), OnFloor(tx, ty))
          }
        }
      }
    }

    var hitFirst = false
    for (y <- 0 until tilesY; x <- 0 until tilesX; if tiles((x, y))) yield {
      val left = rd.connections.get(s"$x,$y left").map(conn).getOrElse {
        if (!tiles((x-1, y))) Wall
        else Internal(s"${rd.name} ${x-1},$y h")
      }
      val right = rd.connections.get(s"$x,$y right").map(conn).getOrElse {
        if (!tiles((x+1, y))) Wall
        else Internal(s"${rd.name} $x,$y h")
      }
      val up = rd.connections.get(s"$x,$y up").map(conn).getOrElse {
        if (!tiles((x, y-1))) Wall
        else Internal(s"${rd.name} $x,${y-1} v")
      }
      val down = rd.connections.get(s"$x,$y down").map(conn).getOrElse {
        if (!tiles((x, y+1))) Wall
        else Internal(s"${rd.name} $x,$y v")
      }
      Room(
        name = rd.name,
        left = left,
        right = right,
        up = up,
        down = down,
        rotatable = rd.rotatable,
        fill = if (!hitFirst) { hitFirst = true; Some(doFill(x, y) _) } else None,
      )
    }
  }

  class RoomTiles(rooms: Seq[Room], sectors: Seq[Sector]) extends GraphTileSet {
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
      else expanded(left).right == expanded(right).left && expanded(left).right != Space

    override def allowedVertical(top: Int, bottom: Int): Boolean =
      if (top == -1) expanded(bottom).up == Space
      else if (bottom == -1) expanded(top).down == Space
      else expanded(top).down == expanded(bottom).up && expanded(top).down != Space

    override def connectedHorizontal(left: Int, right: Int): Boolean =
      expanded(left).right.isConnected

    override def connectedVertical(top: Int, bottom: Int): Boolean =
      expanded(top).down.isConnected

    override def allowedAt(x: Int, y: Int, t: Int): Boolean = {
      val ss = sectors.filter(_.areas.exists(_.contains(x, y)))
      val roomName = expanded(t).name
      ss.exists(s =>
        data.sectors(s.zone).rooms.exists(_.room == roomName))
    }

    override def weight(t: Int): Double = {
      if (expanded(t).name.contains("corridor"))
        0.1
      else
        1
    }
  }

  case class SectorArea(
    x: Int, y: Int, width: Int, height: Int
  ) {
    def contains(tx: Int, ty: Int): Boolean = tx >= x && tx < x + width && ty >= y && ty < y + height
  }
  case class Sector(
    areas: Seq[SectorArea],
    zone: String
  )
  case class ShipSchematic(
    size: (Int, Int),
    sectors: Seq[Sector]
  )

  def generateSchematic(): ShipSchematic = {
    import RandomImplicits._
    // width and height both need to be multiples of 5 for superblocks to work
    // width needs to be a multiple of three to fit the three sectors defined below
    val (width, height) = (5*3*4, 5*3*3)

    val sectors = (for (d <- 0 until 3) yield {
      val l = (width / 3) * d
      val r = l + width / 3
      val cut = random.between((height * 0.3).round.toInt, (height * 0.7).round.toInt)
      Seq(
        Sector(
          zone = if (d == 1) "organics" else "crew",
          areas = Seq(SectorArea(l, 0, r-l, cut + 2))
        ),
        Sector(
          zone = "engineering",
          areas = Seq(SectorArea(l, cut - 2, r-l, height - cut + 2))
        )
      )
    }).flatten ++ Seq(Sector(zone="interdeck", areas = Seq(SectorArea(0,12,width, 5))))

    ShipSchematic(
      size = (width, height),
      sectors = Seq(
        Sector(
          zone = "everything",
          areas = Seq(
            SectorArea(0, 0, width, height)
          )
        )
      ) ++ sectors
    )
  }

  case class Superblock(
    name: String,
    left: Seq[Set[ConnectionType]],
    right: Seq[Set[ConnectionType]],
    top: Seq[Set[ConnectionType]],
    bottom: Seq[Set[ConnectionType]],
  )
  object Superblock {
    def allowedHorizontal(left: Superblock, right: Superblock): Boolean =
      left.right.zip(right.left).forall { case (l, r) => l.exists(r) }
    def allowedVertical(top: Superblock, bottom: Superblock): Boolean =
      top.bottom.zip(bottom.top).forall { case (l, r) => l.exists(r) }
  }

  case class SuperblockTiles(blocks: Seq[Superblock]) extends GraphTileSet {
    override def size: Int = blocks.size

    /** true if |left| can be placed to the left of |right| */
    override def allowedHorizontal(left: Int, right: Int): Boolean =
      if (left < 0) blocks(right).left.forall(_.contains(Wall))
      else if (right < 0) blocks(left).right.forall(_.contains(Wall))
      else Superblock.allowedHorizontal(blocks(left), blocks(right)) && blocks(left).right.forall(_.exists(_ != Space))

    /** true if |top| can be placed above |bottom| */
    override def allowedVertical(top: Int, bottom: Int): Boolean =
      if (top < 0) blocks(bottom).top.forall(_.contains(Space))
      else if (bottom < 0) blocks(top).bottom.forall(_.contains(Space))
      else Superblock.allowedVertical(blocks(top), blocks(bottom)) && blocks(top).bottom.forall(_.exists(_ != Space))

    /** true if the player can navigate from |left| to |right| */
    override def connectedHorizontal(left: Int, right: Int): Boolean =
      blocks(left).right.exists(_.exists(_.isConnected))

    /** true if the player can navigate from |top| to |bottom| */
    override def connectedVertical(top: Int, bottom: Int): Boolean =
      blocks(top).bottom.exists(_.exists(_.isConnected))

    override def allowedAt(x: Int, y: Int, t: Int): Boolean = true
    def interpret(result: Seq[Seq[Int]]): Seq[Seq[Superblock]] = result.map(_.map(blocks))
  }

  val superblockTiles = {
    val wall: Set[ConnectionType] = Set(Wall)
    val space: Set[ConnectionType] = Set(Space)
    val conn: Set[ConnectionType] = Set(Door, Open)
    val `+` = Superblock(
      "+",
      left = Seq(wall, wall, conn, wall, wall),
      right = Seq(wall, wall, conn, wall, wall),
      top = Seq(wall, wall, conn, wall, wall),
      bottom = Seq(wall, wall, conn, wall, wall)
    )
    val `|` = Superblock(
      "|",
      left = Seq(wall, wall, wall, wall, wall),
      right = Seq(wall, wall, wall, wall, wall),
      top = Seq(wall, wall, conn, wall, wall),
      bottom = Seq(wall, wall, conn, wall, wall)
    )
    val `-` = Superblock(
      "-",
      left = Seq(wall, wall, conn, wall, wall),
      right = Seq(wall, wall, conn, wall, wall),
      top = Seq(wall, wall, wall, wall, wall),
      bottom = Seq(wall, wall, wall, wall, wall)
    )
    val T = Superblock(
      "T",
      left = Seq(wall, wall, conn, wall, wall),
      right = Seq(wall, wall, conn, wall, wall),
      top = Seq(wall, wall, wall, wall, wall),
      bottom = Seq(wall, wall, conn, wall, wall)
    )
    val Tup = Superblock(
      "_|_",
      left = Seq(wall, wall, conn, wall, wall),
      right = Seq(wall, wall, conn, wall, wall),
      top = Seq(wall, wall, conn, wall, wall),
      bottom = Seq(wall, wall, wall, wall, wall)
    )
    val Tleft = Superblock(
      "|-",
      left = Seq(wall, wall, wall, wall, wall),
      right = Seq(wall, wall, conn, wall, wall),
      top = Seq(wall, wall, conn, wall, wall),
      bottom = Seq(wall, wall, conn, wall, wall)
    )
    val Tright = Superblock(
      "-|",
      left = Seq(wall, wall, conn, wall, wall),
      right = Seq(wall, wall, wall, wall, wall),
      top = Seq(wall, wall, conn, wall, wall),
      bottom = Seq(wall, wall, conn, wall, wall)
    )
    val spaceUp = Superblock(
      "|",
      left = Seq(wall, wall, wall, wall, wall),
      right = Seq(wall, wall, wall, wall, wall),
      top = Seq(space, space, space, space, space),
      bottom = Seq(wall, wall, conn, wall, wall)
    )
    val spaceDown = Superblock(
      "|",
      left = Seq(wall, wall, wall, wall, wall),
      right = Seq(wall, wall, wall, wall, wall),
      top = Seq(wall, wall, conn, wall, wall),
      bottom = Seq(space, space, space, space, space)
    )

    val blocks = Seq(`+`, `|`, `-`, T, Tleft, Tright, Tup, spaceUp, spaceDown)
    SuperblockTiles(blocks)
  }

  def generateWorld: GameState = generateDetails(generateSchematic())

  def generateDetails(schematic: ShipSchematic): GameState = {
    val (width, height) = schematic.size
    val state = new GameState(data, width * 6, height * 6, new Random(random.nextLong()))
    for ((x, y) <- state.terrain.indices) {
      state.terrain(x, y) = data.terrain("wall")
    }
    val superblocks = WaveFunctionCollapse.graphSolve(superblockTiles, width / 5, height / 5, random, mustConnect = { (a, b) =>
      a._1 == b._1 && a._1 == width / 5 / 2 || (a._2 == b._2 && a._2 > 0 && a._2 < height / 5 - 1)
    })
      .map(superblockTiles.interpret).get
    val tiles = new RoomTiles(rooms, schematic.sectors) {
      override def allowedAt(x: Int, y: Int, t: Int): Boolean = {
        super.allowedAt(x, y, t) && {
          var a = true
          val superblock = superblocks(x / 5)(y / 5)
          if (x % 5 == 0)
            a &&= superblock.left(y % 5).contains(expanded(t).left)
          if (x % 5 == 4)
            a &&= superblock.right(y % 5).contains(expanded(t).right)
          if (y % 5 == 0)
            a &&= superblock.top(x % 5).contains(expanded(t).up)
          if (y % 5 == 4)
            a &&= superblock.bottom(x % 5).contains(expanded(t).down)
          a
        }
      }
    }
    val ss = WaveFunctionCollapse.graphSolve(tiles, width, height, random).map(tiles.interpret).get
    println(s"Room type counts: ${ss.flatten.groupBy(_.name).filter(!_._1.contains("corridor")).mapValues(_.size)}")
    for (ty <- 0 until height; tx <- 0 until width; x = tx * 6; y = ty * 6) {
      val room = ss(tx)(ty)
      // top-left corner
      state.terrain(x, y) = data.terrain("wall")
      // top wall
      room.up match {
        case Wall =>
          for (dx <- 1 to 5) {
            state.terrain(x + dx, y) = data.terrain("wall")
          }
        case Door =>
          for (dx <- 1 to 5) {
            state.terrain(x + dx, y) = data.terrain("wall")
          }
          state.terrain(x + 3, y) = data.terrain("floor")
          generateItem(data.itemGroups("automatic door")).foreach(state.items.put(_, OnFloor(x + 3, y)))
        case Open | Internal(_, _) =>
          for (dx <- 1 to 5) {
            state.terrain(x + dx, y) = data.terrain("floor")
          }
        case Space =>
          for (dx <- 1 to 5) {
            state.terrain(x + dx, y) = data.terrain("empty space")
          }
      }
      // left wall
      room.left match {
        case Wall =>
          for (dy <- 1 to 5) {
            state.terrain(x, y + dy) = data.terrain("wall")
          }
        case Door =>
          for (dy <- 1 to 5) {
            state.terrain(x, y + dy) = data.terrain("wall")
          }
          state.terrain(x, y + 3) = data.terrain("floor")
          generateItem(data.itemGroups("automatic door")).foreach(state.items.put(_, OnFloor(x, y + 3)))
        case Open | Internal(_, _) | Space =>
          for (dy <- 1 to 5) {
            state.terrain(x, y + dy) = data.terrain("floor")
          }
      }
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
    val (startingRoomX, startingRoomY) = random.pick(for {
      (cols, tx) <- ss.zipWithIndex
      (room, ty) <- cols.zipWithIndex
      if room.name == "cryopods"
    } yield (tx, ty))

    state.movePlayer(startingRoomX*6 + 3, startingRoomY*6 + 3)
    state
  }

  def generateItem(itemGroup: ItemGroup): Seq[Item] = {
    itemGroup.choose.sample()(random, data.itemGroups.mapValues(_.choose)).map { itemId =>
      data.items(itemId).generateItem()
    }
  }
}
