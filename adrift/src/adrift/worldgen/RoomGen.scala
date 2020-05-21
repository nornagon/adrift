package adrift.worldgen

import adrift.Population.Table
import adrift.RandomImplicits._
import adrift.worldgen.WaveFunctionCollapse.GraphTileSet
import adrift._
import io.circe.{Decoder, HCursor, Json, JsonObject}

import scala.util.Random

trait RoomGen {
  def generate(state: GameState, levelId: LevelId, cells: Seq[(Int, Int)])(implicit r: Random): Unit
}

case class FurnishItem(
  `type`: Table[String],
  wall_adjacent: Boolean = false,
  nearby: Option[Table[String]]
)

case class Furnish(items: Table[FurnishItem]) extends RoomGen {
  override def generate(state: GameState, levelId: LevelId, cells: Seq[(Int, Int)])(implicit r: Random): Unit = {
    val cellSet = cells.toSet
    def isWallAdjacent(p: (Int, Int)): Boolean = {
      val (x, y) = p
      !cellSet(x-1, y) || !cellSet(x+1, y) || !cellSet(x, y-1) || !cellSet(x, y+1)
    }
    def isEmpty(p: (Int, Int)): Boolean = state.items.lookup(OnFloor(Location(levelId, p._1, p._2))).isEmpty
    def chooseWallAdjacentLocation(): Option[(Int, Int)] = {
      val candidates = cells filter isWallAdjacent filter isEmpty
      if (candidates.nonEmpty) Some(r.pick(candidates)) else None
    }
    def chooseAnyLocation(): Option[(Int, Int)] = {
      val candidates = cells filter isEmpty
      if (candidates.nonEmpty) Some(r.pick(candidates)) else None
    }
    def chooseNearbyLocation(loc: (Int, Int)): Option[(Int, Int)] = {
      val (x, y) = loc
      val candidates = cellSet intersect Set((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)) filter isEmpty
      if (candidates.nonEmpty) Some(r.pick(candidates)) else None
    }
    val toPlace = items.sample()(r, Map.empty)
    for (item <- toPlace) {
      val maybeLoc = if (item.wall_adjacent) chooseWallAdjacentLocation() else chooseAnyLocation()
      maybeLoc foreach { loc =>
        val items = state.sampleItem(item.`type`)
        for (i <- items) state.items.put(i, OnFloor(Location(levelId, loc._1, loc._2)))
        for (nearby <- item.nearby; nearbyLoc <- chooseNearbyLocation(loc)) {
          val items = state.sampleItem(nearby)
          for (i <- items) state.items.put(i, OnFloor(Location(levelId, nearbyLoc._1, nearbyLoc._2)))
        }
      }
    }
  }
}

case class PaletteDef(terrain: Option[String] = None, items: Option[Table[String]] = None)
case class PartWithOpts(part: String, min: Option[Int] = None, max: Option[Int] = None)
object PartWithOpts {
  import io.circe.generic.semiauto._
  private val derivedDecoder = deriveDecoder[PartWithOpts]
  private def decodeFromString[T: Decoder]: Decoder[PartWithOpts] = (c: HCursor) =>
    for { part <- c.as[String] } yield PartWithOpts(part)
  implicit val decoder: Decoder[PartWithOpts] = decodeFromString or derivedDecoder
}
case class WFC(parts: Seq[PartWithOpts], defs: Map[String, PaletteDef]) extends RoomGen {
  sealed trait AdjacencyType {
    def rotated: AdjacencyType = this
  }
  case class Matching(c: Char) extends AdjacencyType
  case class Internal(s: String, r: Int = 0) extends AdjacencyType {
    override def rotated: AdjacencyType = copy(r = (r + 1) % 4)
  }
  case object Any extends AdjacencyType

  parts.view.flatMap(_.part.toCharArray).to(Set).filterNot(c => c == 'x' || c == '\n') foreach { c =>
    assert(defs.contains(c.toString), s"Expected defs to contain '$c'")
  }

  case class Tile(
    value: Char,
    left: AdjacencyType,
    right: AdjacencyType,
    up: AdjacencyType,
    down: AdjacencyType,
    partId: Option[Int] = None,
  ) {
    override def toString: String = {
      s"T[$value, l=$left, r=$right, u=$up, d=$down p=${partId.getOrElse('?')}]"
    }
    def rotated: Tile = copy(
      left = up.rotated,
      up = right.rotated,
      right = down.rotated,
      down = left.rotated,
    )
  }

  private def tilesFromPart(part: PartWithOpts, i: Int) = {
    val lines = part.part.split("\n")
    // for now, only support rectangles
    val width = lines.head.length
    val height = lines.length
    assert(lines.forall(l => l.length == width), "WFC parts must be rectangular for now")
    val grid = new Grid(width, height)(' ')
    for (y <- 0 until height; x <- 0 until width)
      grid(x, y) = lines(y)(x)
    val tiles = for (y <- 1 until height - 1; x <- 1 until width - 1) yield {
      Tile(
        partId = if (y == 1 && x == 1) Some(i) else None,
        value = grid(x, y),
        left = if (x == 1) Matching(grid(x - 1, y)) else Internal(s"Part $i ${x - 1},$y h"),
        right = if (x == width - 2) Matching(grid(x + 1, y)) else Internal(s"Part $i $x,$y h"),
        up = if (y == 1) Matching(grid(x, y - 1)) else Internal(s"Part $i $x,${y - 1} v"),
        down = if (y == height - 2) Matching(grid(x, y + 1)) else Internal(s"Part $i $x,$y v"),
      )
    }
    tiles ++ tiles.map(_.rotated) ++ tiles.map(_.rotated.rotated) ++ tiles.map(_.rotated.rotated.rotated)
  }

  private val partTiles: Seq[Tile] = for {
    (part, i) <- parts.zipWithIndex
    tile <- tilesFromPart(part, i)
  } yield tile

  private val missingTiles = {
    partTiles.zipWithIndex.flatMap { case (t, i) =>
      // if there's no tile that could possibly match |t| to the left...
      (t.left match {
        case Matching(c) if c != 'x' && (!partTiles.exists(matchesHorizontal(_, t))) =>
          // generate a new tile that will.
          println(s"Tile $t was not matchable on the left")
          Seq(Tile(c, Any, right = Matching(t.value), Any, Any))
        case _ => Seq.empty
      }) ++ (t.right match {
        case Matching(c) if c != 'x' && (!partTiles.exists(matchesHorizontal(t, _))) =>
          Seq(Tile(c, left = Matching(t.value), Any, Any, Any))
        case _ => Seq.empty
      }) ++ (t.up match {
        case Matching(c) if c != 'x' && (!partTiles.exists(matchesVertical(_, t))) =>
          Seq(Tile(c, Any, Any, Any, down = Matching(t.value)))
        case _ => Seq.empty
      }) ++ (t.down match {
        case Matching(c) if c != 'x' && (!partTiles.exists(matchesVertical(t, _))) =>
          Seq(Tile(c, Any, Any, up = Matching(t.value), Any))
        case _ => Seq.empty
      })
    }
  }.distinctBy(t => (t.left, t.right, t.up, t.down, t.value))
  private val allTiles = partTiles ++ missingTiles

  def matchesHorizontal(left: Tile, right: Tile): Boolean = {
    (left.right, right.left) match {
      case (Any, Matching(x)) => left.value == x
      case (Matching(x), Any) => right.value == x
      case (Any, Any) => true
      case (lr: Internal, rl: Internal) => lr == rl
      case (lr: Matching, rl: Matching) => right.value == lr.c && left.value == rl.c
      case _ => false
    }
  }

  def matchesVertical(top: Tile, bottom: Tile): Boolean = {
    (top.down, bottom.up) match {
      case (Any, Matching(x)) => top.value == x
      case (Matching(x), Any) => bottom.value == x
      case (Any, Any) => true
      case (ud: Internal, du: Internal) => ud == du
      case (ud: Matching, du: Matching) => bottom.value == ud.c && top.value == du.c
      case _ => false
    }
  }

  private lazy val gts = new GraphTileSet {
    override def size: Int = allTiles.size

    /** true if |left| can be placed to the left of |right| */
    override def allowedHorizontal(left: Int, right: Int): Boolean = {
      if (left < 0) return allTiles(right).left == Matching('x')
      if (right < 0) return allTiles(left).right == Matching('x')
      matchesHorizontal(allTiles(left), allTiles(right))
    }

    /** true if |top| can be placed above |bottom| */
    override def allowedVertical(top: Int, bottom: Int): Boolean = {
      if (top < 0) return allTiles(bottom).up == Matching('x')
      if (bottom < 0) return allTiles(top).down == Matching('x')
      matchesVertical(allTiles(top), allTiles(bottom))
    }

    /** true if the player can navigate from |left| to |right| */
    override def connectedHorizontal(left: Int, right: Int): Boolean = true

    /** true if the player can navigate from |top| to |bottom| */
    override def connectedVertical(top: Int, bottom: Int): Boolean = true

    override def allowedAt(x: Int, y: Int, t: Int): Boolean = true

    override def countConstraints: Iterable[WaveFunctionCollapse.CountConstraint] = {
      for (i <- parts.indices; if parts(i).min.nonEmpty || parts(i).max.nonEmpty) yield {
        val representatives = allTiles.indices.filter(t => allTiles(t).partId.contains(i))
        val lb = parts(i).min.getOrElse(0)
        val ub = parts(i).max.getOrElse(Integer.MAX_VALUE)
        WaveFunctionCollapse.CountConstraint(representatives, lb, ub)
      }
    }
  }

  override def generate(state: GameState, levelId: LevelId, cells: Seq[(Int, Int)])(implicit r: Random): Unit = {
    val xmin = cells.view.map(_._1).min
    val xmax = cells.view.map(_._1).max
    val ymin = cells.view.map(_._2).min
    val ymax = cells.view.map(_._2).max
    val width = xmax - xmin + 1
    val height = ymax - ymin + 1
    assert(cells.size == width * height, "WFC only supports rectangular rooms currently")
    WaveFunctionCollapse.graphSolve(gts, width, height, r) match {
      case Some(result) =>
        for (y <- 0 until height; x <- 0 until width) {
          val tile = allTiles(result(x)(y))
          val paletteDef = defs(tile.value.toString)
          val terrain = paletteDef.terrain
          val tx = xmin + x
          val ty = ymin + y
          state.levels(levelId).terrain(tx, ty) = state.data.terrain(terrain.getOrElse("floor"))
          paletteDef.items.foreach { table =>
            state.sampleItem(table) foreach {
              state.items.put(_, OnFloor(Location(levelId, tx, ty)))
            }
          }
        }
      case None =>
        throw new RuntimeException(s"Failed to generate $width x $height room")
    }
  }
}

object RoomGen {
  import cats.syntax.functor._
  import io.circe.generic.extras.Configuration
  import io.circe.generic.extras.auto._
  import io.circe.{Decoder, Encoder}
  implicit private val configuration: Configuration = Configuration.default.withDefaults.withDiscriminator("type")

  val decoders: Map[String, Decoder[RoomGen]] = Map(
    "Furnish" -> Decoder[Furnish].widen,
    "WFC" -> Decoder[WFC].widen,
  )

  implicit val encodeBehavior: Encoder[RoomGen] = (b: RoomGen) => Json.fromJsonObject(JsonObject.singleton(b.getClass.getSimpleName, b match {
    case b: Furnish => Encoder[Furnish].apply(b)
  }))
}
