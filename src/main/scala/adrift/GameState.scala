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
  val items: Grid[Seq[Item]] = new Grid[Seq[Item]](width, height)(Seq.empty)
  var player: (Int, Int) = (0, 0)
  val hands = Container(maxItems = 2)

  var message: Option[String] = None

  def receive(action: Action): Unit = {
    message = None
    action match {
      case PlayerMove(dx, dy) =>
        if (map(player._1 + dx, player._2 + dy).walkable || true) {
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
    recalculateFOV()
  }

  private var visible = Set.empty[(Int, Int)]
  private def recalculateFOV(): Unit = {
    val newVisible = mutable.Set.empty[(Int, Int)]
    newVisible += player
    val opaque = (dx: Int, dy: Int) => !map.get(player._1 + dx, player._2 + dy).exists(_.walkable)
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
  def generateWorld: GameState = {
    val state = new GameState(2048, 128)
    val random = new scala.util.Random(42)

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
        }.find(state.map(_).walkable).get
      def findFirstWall(p: (Int, Int), d: (Int, Int)): Option[(Int, Int)] =
        Stream.iterate(p)(a => (a._1 + d._1, a._2 + d._2))
          .takeWhile(state.map.contains)
          .find(p => state.map(p) == Terrain.Wall)
      def clipX(x: Int) = math.max(0, math.min(state.map.width - 1, x))
      def clipY(y: Int) = math.max(0, math.min(state.map.height - 1, y))
      def clip(p: (Int, Int)): (Int, Int) = (clipX(p._1), clipY(p._2))
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
            // walk left until we find a wall.
            findFirstWall(empty, (-1, 0)).map { e =>
              (e._1 - width, e._2 - (height * random.nextFloat()).toInt) }
          } else {
            // +x, empty space to our left.
            findFirstWall(empty, (1, 0)).map { e =>
              (e._1 + 1, e._2 - (height * random.nextFloat()).toInt) }
          }
        } else {
          // we'll be -y or +y
          if (random.nextBoolean()) {
            // -y, empty space to our down.
            findFirstWall(empty, (0, -1)).map { e =>
              (e._1 - (width * random.nextFloat()).toInt, e._2 - height) }
          } else {
            // +y, empty space to our up.
            findFirstWall(empty, (0, 1)).map { e =>
              (e._1 - (width * random.nextFloat()).toInt, e._2 + 1) }
          }
        }
        if (rectLeftTop.isEmpty) fails += 1
        rectLeftTop foreach { rlt =>
          val canPlace = (for (dx <- 0 until width; dy <- 0 until height) yield (dx, dy)).forall { case (dx, dy) =>
            state.map(clip(rlt._1 + dx, rlt._2 + dy)) == Terrain.Wall
          }
          if (canPlace) {
            for (x <- rlt._1 until (rlt._1 + width); y <- rlt._2 until rlt._2 + height) {
              state.map(clip((x, y))) = Terrain.Floor
            }
          } else {
            fails += 1
          }
        }
      }
      println(s"Fails: $fails")
    }

    /*
    GridUtils.filledCircle(64, 64, r = 50) { (x, y) =>
      state.map(x, y) = if (random.nextFloat() < 0.1) Terrain.Grass else Terrain.Floor
    }
    GridUtils.hollowCircle(64, 64, r = 50) { (x, y) => state.map(x, y) = Terrain.GlassWall }
    for ((x, y) <- state.map.indices) {
      if (state.map(x, y) == Terrain.Grass && random.nextFloat() < 1/6f)
        state.map(x, y) = if (random.nextFloat() < 0.3) Terrain.TreeFicus else Terrain.TreeOak
      if (state.map(x, y).walkable) {
        if (random.nextFloat() < 1/512f) {
          state.items(x, y) :+= generateItem(items.HoloNote)
        }
      }
    }
    */
    state.player = (64, 32)
    state
  }

  def generateItem(itemKind: ItemKind): Item = {
    Item(itemKind, Seq.empty, itemKind.parts.flatMap { case (part, count) => Seq.fill(count)(generateItem(part)) })
  }
}