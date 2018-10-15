package adrift

import adrift.Action._
import adrift.items.{Item, ItemKind}

import scala.collection.mutable

sealed trait ItemLocation
case class OnFloor(x: Int, y: Int/* TODO: level? */, index: Int) extends ItemLocation
case class InHands(index: Int) extends ItemLocation


case class Container(maxItems: Int, contents: mutable.Buffer[Item] = mutable.Buffer.empty)


class GameState(width: Int, height: Int) {
  val map: Grid[Terrain] = new Grid[Terrain](width, height)(Terrain.EmptySpace)
  val furniture: Grid[Option[Furniture]] = new Grid[Option[Furniture]](width, height)(None)
  val items: Grid[Seq[Item]] = new Grid[Seq[Item]](width, height)(Seq.empty)
  var player: (Int, Int) = (0, 0)
  val hands = Container(maxItems = 2)

  var message: Option[String] = None

  def receive(action: Action): Unit = {
    message = None
    action match {
      case PlayerMove(dx, dy) =>
        if (canWalk(player._1 + dx, player._2 + dy)) {
          movePlayer(player._1 + dx, player._2 + dy)
        }
      case Disassemble(location) =>
        val item = removeItem(location)
        items(player) ++= item.parts
        message = Some(s"You take apart the ${item.kind.name}.")
      case PickUp(location) =>
        if (hands.contents.size < hands.maxItems) {
          val item = removeItem(location)
          hands.contents.append(item)
          message = Some(s"You pick up the ${item.kind.name}.")
        } else {
          message = Some("Your hands are full.")
        }
      case PutDown(location) =>
        location match {
          case loc: InHands =>
            val item = removeItem(loc)
            items(player) :+= item
            message = Some(s"You place the ${item.kind.name} on the ${map(player)}.")
          case _ =>
            message = Some("You can't put that down.")
        }
      case Quit =>
    }
  }

  def movePlayer(x: Int, y: Int): Unit = {
    player = (x, y)
    openNearbyDoors()
    recalculateFOV()
  }

  def openNearbyDoors(): Unit = {
    for (x <- player._1 - 6 to player._1 + 6; y <- player._2 - 6 to player._2 + 6) {
      for (f <- furniture.get(x, y).flatten) {
        if (f == Furniture.DoorClosed && (x - player._1).abs < 2 && (y - player._2).abs < 2) {
          furniture(x, y) = Some(Furniture.DoorOpen)
        } else if (f == Furniture.DoorOpen && ((x - player._1).abs > 2 || (y - player._2).abs > 2)) {
          furniture(x, y) = Some(Furniture.DoorClosed)
        }
      }
    }
  }

  def canWalk(x: Int, y: Int): Boolean = {
    map.get(x, y).exists(_.walkable) && furniture.get(x, y).flatten.forall(_.walkable)
  }

  def isOpaque(x: Int, y: Int): Boolean = {
    map.get(x, y).exists(_.opaque) || furniture.get(x, y).flatten.exists(_.opaque)
  }

  private var visible = Set.empty[(Int, Int)]
  private def recalculateFOV(): Unit = {
    val newVisible = mutable.Set.empty[(Int, Int)]
    newVisible += player
    val opaque = (dx: Int, dy: Int) => isOpaque(player._1 + dx, player._2 + dy)
    FOV.castShadows(radius = 100, opaqueApply = true, opaque, (x, y) => {
      newVisible.add((player._1 + x, player._2 + y))
    })
    visible = newVisible.toSet
  }
  def isVisible(x: Int, y: Int): Boolean = isVisible((x, y))
  def isVisible(p: (Int, Int)): Boolean = visible contains p

  def itemAtLocation(location: ItemLocation): Item = location match {
    case OnFloor(x, y, index) =>
      items(x, y)(index)
    case InHands(index) =>
      hands.contents(index)
  }

  def removeItem(location: ItemLocation): Item = location match {
    case OnFloor(x, y, index) =>
      val item = items(x, y)(index)
      items(x, y) = items(x, y).patch(index, Nil, 1)
      item
    case InHands(index) =>
      val item = hands.contents(index)
      hands.contents.remove(index)
      item
  }
}

object GameState {
  def generateWorld3: GameState = {
    val random = new scala.util.Random(42)
    val state = new GameState(2048, 128)
    for ((x, y) <- state.map.indices) {
      state.map(x, y) = Terrain.Wall
    }
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
          state.furniture(x*6 + 3, y * 6 + 6) = Some(Furniture.DoorClosed)
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
          state.furniture(x*6 + 6, y * 6 + 3) = Some(Furniture.DoorClosed)
        }
      }
    }

    state.movePlayer(4, state.map.height / 2)

    state
  }


  def generateWorld: GameState = {
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
    Item(itemKind, Seq.empty, itemKind.parts.flatMap { case ((part, count), operation) => Seq.fill(count)(generateItem(part)) })
  }
}