package adrift.worldgen

import adrift._
import adrift.items.{Item, ItemKind}
import adrift.worldgen.WaveFunctionCollapse.GraphTileSet

import scala.collection.mutable
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

  def generateWorld2: GameState = {
    val random = new scala.util.Random(42)
    // let's not worry, for now, about the content of the rooms. just the shapes.

    // so step one, let's make a lattice of points, where each point is going to be room-potential.
    val rooms = new Grid[Int](width = 200, height = 15)(initial = 0)
    val connections = mutable.Map.empty[((Int, Int), (Int, Int)), Int]
    def addConn(from: (Int, Int), to: (Int, Int), typ: Int): Unit = {
      connections((from, to)) = typ
      connections((to, from)) = typ
    }
    /*
     room door
   -1  0  0
    +-----+  door y-1
    |     |
    |  +  >  room y=0
    |     |
    +--v--+  door y=0

    +-----+-----+-----+-----+
    |     |     |     |     |
    |     0     1     2     |
    |     |     |     |     |
    +--0--+--0--+--0--+--0--+
    |     |     |     |     |
    |     0     1     2     |
    |     |     |     |     |
    +--1--+--1--+--1--+--1--+
    |     |     |     |     |
    |     0     1     2     |
    |     |     |     |     |
    +--2--+--2--+--2--+--2--+
    |     |     |     |     |
    |     0     1     2     |
    |     |     |     |     |
    +-----+-----+-----+-----+

    number h doors = number h rooms * (number v rooms - 1)
    number v doors = number v rooms * (number h rooms - 1)
    total # doors = h doors + v doors
                  = h rooms * (v rooms - 1) + v rooms * (h rooms - 1)
                  = h rooms * v rooms - h rooms + v rooms * h rooms - v rooms
                  = 2 * h * v - h - v
                  = 2 s s - 2 s
                  = 2 s (s - 1)

     */
    // set the center line to be corridor
    for (x <- 0 until rooms.width) {
      rooms(x, rooms.height/2) = 1
      addConn((x, rooms.height/2), (x+1, rooms.height/2), 1)
    }

    for (i <- 1 to 2000) {
      val x = random.nextInt(rooms.width-1)
      val y = random.nextInt(rooms.height-1)
      val mirrorY = -y + 2 * (rooms.height / 2)
      val connType = random.nextInt(2) + 1
      if (random.nextBoolean()) {
        addConn((x, y), (x+1, y), connType)
        addConn((x, mirrorY), (x+1, mirrorY), connType)
      } else {
        addConn((x, y), (x, y+1), connType)
        addConn((x, mirrorY), (x, mirrorY-1), connType)
      }
    }

    // everything but the main corridor will be empty.

    // now let's turn that into a map.

    val state = new GameState(rooms.width * 6, rooms.height * 6)
    for (y <- 0 until rooms.height; x <- 0 until rooms.width) {
      for (dy <- 1 to 5; dx <- 1 to 5) {
        state.map(x*6 + dx, y*6 + dy) = Terrain.Floor
      }
    }

    // walls
    for (y <- 0 until rooms.height-1; x <- 0 until rooms.width-1) {
      state.map(x*6, y*6) = Terrain.Wall;
      {
        val c = connections getOrElse(((x, y), (x, y + 1)), 0)
        val t = c match {
          case 0 => Terrain.Wall
          case 1 => Terrain.Floor
          case 2 => Terrain.Wall
        }
        for (dx <- 1 to 5) {
          state.map(x*6 + dx, y*6 + 6) = t
        }
        if (c == 2) {
          state.map(x*6 + 3, y * 6 + 6) = Terrain.Floor
          state.furniture(x*6 + 3, y * 6 + 6) = Some(Furniture.AutomaticDoor())
        }
      }
      {
        val c = connections getOrElse(((x, y), (x + 1, y)), 0)
        val t = c match {
          case 0 => Terrain.Wall
          case 1 => Terrain.Floor
          case 2 => Terrain.Wall
        }
        for (dy <- 1 to 5) {
          state.map(x * 6 + 6, y * 6 + dy) = t
        }
        if (c == 2) {
          state.map(x*6 + 6, y * 6 + 3) = Terrain.Floor
          state.furniture(x*6 + 6, y * 6 + 3) = Some(Furniture.AutomaticDoor())
        }
      }
    }

    state.movePlayer(4, state.map.height / 2)

    state
  }


  def generateWorld3: GameState = {
    val state = new GameState(2048, 128)
    val random = new scala.util.Random(42)

    /*
    for (y <- 0 until state.map.height; x <- 0 until state.map.width) {
      state.map(x, y) = Terrain.Wall
    }

    // Main corridor around the ring
    val nPoints = state.map.width / 128
    val pointXs = (0 until nPoints) map { x =>
      math.min(math.max(x * 128 + random.nextGaussian() * 16, 0), state.map.width - 1).toInt
    }
    val pointYs = (0 until nPoints) map { _ =>
      val h = random.nextGaussian() * state.map.height * 0.2 + state.map.height * 0.5
      math.round(math.min(math.max(h, 0), state.map.height - 1)).toInt
    }
    val points = pointXs zip pointYs
    for (Seq((x1, y1), (x2, y2)) <- points.sliding(2)) {
      for (x <- x1 to x2) {
        state.map(x, y1-1) = Terrain.Floor
        state.map(x, y1) = Terrain.Floor
        state.map(x, y1+1) = Terrain.Floor
      }
      for (y <- math.min(y1, y2) to math.max(y1, y2)) {
        state.map(x2-1, y) = Terrain.Floor
        state.map(x2, y) = Terrain.Floor
        state.map(x2+1, y) = Terrain.Floor
      }
    }
    for (segment <- 0 until 16) {
      val xMin = segment * (state.map.width / 16)
      val xMax = (segment + 1) * (state.map.width / 16)
      def findEmptySpace: (Int, Int) =
        Stream.continually {
          val x = random.nextInt(xMax - xMin) + xMin
          val y = random.nextInt(state.map.height)
          (x, y)
        }.find(p => state.canWalk(p._1, p._2)).get
      def findFirstWall(p: (Int, Int), d: (Int, Int)): Option[(Int, Int)] =
        Stream.iterate(p)(a => (a._1 + d._1, a._2 + d._2))
          .takeWhile(state.map.contains)
          .find(p => state.map(p) == Terrain.Wall)
      var fails = 0
      for (attempt <- 1 to 256) {
        // find something to attach to
        val empty = findEmptySpace
        val width = math.round(math.max(2, random.nextGaussian() * 5 + 10)).toInt
        val height = math.round(math.max(2, random.nextGaussian() * 5 + 10)).toInt
        // pick a rectangle with the empty space at one of its borders
        val rectLeftTop = if (random.nextBoolean()) {
          // we'll be -x or +x
          if (random.nextBoolean()) {
            // -x, empty space to our right.
            findFirstWall(empty, (-1, 0)).map { e =>
              ((e._1 - width, e._2 - (height * random.nextFloat()).toInt), e) }
          } else {
            // +x, empty space to our left.
            findFirstWall(empty, (1, 0)).map { e =>
              ((e._1 + 1, e._2 - (height * random.nextFloat()).toInt), e) }
          }
        } else {
          // we'll be -y or +y
          if (random.nextBoolean()) {
            // -y, empty space to our down.
            findFirstWall(empty, (0, -1)).map { e =>
              ((e._1 - (width * random.nextFloat()).toInt, e._2 - height), e) }
          } else {
            // +y, empty space to our up.
            findFirstWall(empty, (0, 1)).map { e =>
              ((e._1 - (width * random.nextFloat()).toInt, e._2 + 1), e) }
          }
        }
        if (rectLeftTop.isEmpty) fails += 1
        rectLeftTop foreach { case (rlt, door) =>
          val canPlace = (for (dx <- -1 to width; dy <- -1 to height) yield (rlt._1 + dx, rlt._2 + dy)).forall { p =>
            state.map.get(p).contains(Terrain.Wall)
          }
          if (canPlace) {
            for (x <- rlt._1 until (rlt._1 + width); y <- rlt._2 until rlt._2 + height) {
              state.map(x, y) = Terrain.Floor
            }
            state.map(door) = Terrain.Floor
            state.furniture(door) = Some(Furniture.DoorClosed)
          } else {
            fails += 1
          }
        }
      }
      println(s"Fails: $fails")
    }
    */

    GridUtils.filledCircle(64, 64, r = 50) { (x, y) =>
      state.map(x, y) = if (random.nextFloat() < 0.1) Terrain.Grass else Terrain.Floor
    }
    GridUtils.hollowCircle(64, 64, r = 50) { (x, y) => state.map(x, y) = Terrain.GlassWall }
    for ((x, y) <- state.map.indices) {
      if (state.map(x, y) == Terrain.Grass && random.nextFloat() < 1/6f)
        state.map(x, y) = if (random.nextFloat() < 0.3) Terrain.TreeFicus else Terrain.TreeOak
      if (state.map(x, y).walkable) {
        if (random.nextFloat() < 1/512f) {
          //          state.items(x, y) :+= generateItem(items.HoloNote)
          state.items(x, y) :+= generateItem(items.LaserPump)
        }
      }
    }
    state.movePlayer(64, 32)
    state
  }

  def generateItem(itemKind: ItemKind): Item = {
    Item(itemKind, Seq.empty, itemKind.parts.flatMap { case (part, count) => Seq.fill(count)(generateItem(part)) })
  }
}
