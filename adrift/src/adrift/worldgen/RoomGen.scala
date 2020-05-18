package adrift.worldgen

import java.nio.file.Paths

import adrift.{Data, GameState, Grid, LevelId, Location, OnFloor}
import adrift.Population.Table
import io.circe.{Json, JsonObject}
import adrift.RandomImplicits._
import adrift.worldgen.WaveFunctionCollapse.GraphTileSet

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
case class WFC(parts: Seq[String], defs: Map[String, PaletteDef]) extends RoomGen {
  var name: String = "unknown"
  sealed trait AdjacencyType
  case class Matching(c: Char) extends AdjacencyType
  case class Internal(s: String) extends AdjacencyType

  parts.view.flatMap(_.toCharArray).to(Set).filterNot(c => c == 'x' || c == '\n') foreach { c =>
    assert(defs.contains(c.toString), s"Expected defs to contain '$c'")
  }

  case class Tile(
    name: String,
    value: Char,
    left: AdjacencyType,
    right: AdjacencyType,
    up: AdjacencyType,
    down: AdjacencyType,
  ) {
    override def toString: String = {
      s"T[$value, l=$left, r=$right, u=$up, d=$down]"
    }
    def rotated: Tile = copy(
      left = up,
      up = right,
      right = down,
      down = left,
    )
  }
  private lazy val allTiles = for {
    (part, i) <- parts.zipWithIndex
    tile <- {
      val lines = part.split("\n")
      // for now, only support rectangles
      val width = lines.head.length
      val height = lines.length
      assert(lines.forall(l => l.length == width), s"WFC parts must be rectangular")
      val grid = new Grid(width, height)(' ')
      for (y <- 0 until height; x <- 0 until width)
        grid(x, y) = lines(y)(x)
      val tiles = for (y <- 1 until height - 1; x <- 1 until width - 1) yield {
        Tile(
          s"Part $i at $x,$y",
          value = grid(x, y),
          left = if (x == 1) Matching(grid(x - 1, y)) else Internal(s"Part $i ${x-1},$y h"),
          right = if (x == width - 2) Matching(grid(x + 1, y)) else Internal(s"Part $i $x,$y h"),
          up = if (y == 1) Matching(grid(x, y - 1)) else Internal(s"Part $i $x,${y-1} v"),
          down = if (y == height - 2) Matching(grid(x, y + 1)) else Internal(s"Part $i $x,$y v"),
        )
      }
      tiles ++ tiles.map(_.rotated) ++ tiles.map(_.rotated.rotated) ++ tiles.map(_.rotated.rotated.rotated)
    }
  } yield tile

  val gts = new GraphTileSet {
    override def size: Int = allTiles.size

    /** true if |left| can be placed to the left of |right| */
    override def allowedHorizontal(left: Int, right: Int): Boolean = {
      if (left < 0) return allTiles(right).left == Matching('x')
      if (right < 0) return allTiles(left).right == Matching('x')
      val leftTile = allTiles(left)
      val rightTile = allTiles(right)
      (leftTile.right, rightTile.left) match {
        case (lr: Internal, rl: Internal) => lr == rl
        case (lr: Matching, rl: Matching) => rightTile.value == lr.c && leftTile.value == rl.c
        case _ => false
      }
    }

    /** true if |top| can be placed above |bottom| */
    override def allowedVertical(top: Int, bottom: Int): Boolean = {
      if (top < 0) return allTiles(bottom).up == Matching('x')
      if (bottom < 0) return allTiles(top).down == Matching('x')
      val topTile = allTiles(top)
      val bottomTile = allTiles(bottom)
      (topTile.down, bottomTile.up) match {
        case (ud: Internal, du: Internal) => ud == du
        case (ud: Matching, du: Matching) => bottomTile.value == ud.c && topTile.value == du.c
        case _ => false
      }
    }

    /** true if the player can navigate from |left| to |right| */
    override def connectedHorizontal(left: Int, right: Int): Boolean = true

    /** true if the player can navigate from |top| to |bottom| */
    override def connectedVertical(top: Int, bottom: Int): Boolean = true

    override def allowedAt(x: Int, y: Int, t: Int): Boolean = true
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
            val items = table.sample()(r, state.data.itemGroups.view.mapValues(_.choose))
            items.foreach { item_kind_id =>
              val itemKind = state.data.items(item_kind_id)
              state.items.put(itemKind.generateItem(), OnFloor(Location(levelId, tx, ty)))
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
  import io.circe.{Decoder, Encoder}
  import io.circe.generic.extras.Configuration
  import io.circe.generic.extras.auto._
  import io.circe.generic.extras.semiauto._
  import adrift.Population.serialization._
  implicit private val configuration: Configuration = Configuration.default.withDefaults.withDiscriminator("type")

  val decoders: Map[String, Decoder[RoomGen]] = Map(
    "Furnish" -> Decoder[Furnish].widen,
    "WFC" -> Decoder[WFC].widen,
  )

  implicit val encodeBehavior: Encoder[RoomGen] = (b: RoomGen) => Json.fromJsonObject(JsonObject.singleton(b.getClass.getSimpleName, b match {
    case b: Furnish => Encoder[Furnish].apply(b)
  }))
}
