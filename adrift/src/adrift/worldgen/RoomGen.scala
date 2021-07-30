package adrift.worldgen

import adrift.Population.Table
import adrift.RandomImplicits._
import adrift.YamlObject.ItemWithExtras
import adrift.worldgen.WaveFunctionCollapse.GraphTileSet
import adrift._
import adrift.items.behaviors.{HasPorts, LayerSet}
import io.circe.{Decoder, HCursor, Json, JsonObject}

import scala.util.Random

case class RoomGen(
  algorithm: RoomGenAlgorithm,
  minArea: Int,
  maxArea: Int,
)

trait RoomGenAlgorithm {
  def generate(state: GameState, levelId: LevelId, cells: Seq[(Int, Int)])(implicit r: Random): Unit
}

case class FurnishItem(
  `type`: Table[String],
  wall_adjacent: Boolean = false,
  nearby: Option[Table[String]]
)

case class Furnish(items: Table[FurnishItem]) extends RoomGenAlgorithm {
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
        val items = state.sampleItem(item.`type`.map(ItemWithExtras(_)))
        for (i <- items) state.items.put(i, OnFloor(Location(levelId, loc._1, loc._2)))
        for (nearby <- item.nearby; nearbyLoc <- chooseNearbyLocation(loc)) {
          val items = state.sampleItem(nearby.map(ItemWithExtras(_)))
          for (i <- items) state.items.put(i, OnFloor(Location(levelId, nearbyLoc._1, nearbyLoc._2)))
        }
      }
    }
  }
}

case class PaletteDef(terrain: Option[String] = None, items: Option[Table[YamlObject.ItemWithExtras]] = None)
case class PartWithOpts(
  part: String,
  min: Option[Int] = None,
  max: Option[Int] = None,
  weight: Option[Double] = None,
  rotate: Option[Boolean] = None,
  flip: Option[Boolean] = None
)
object PartWithOpts {
  import io.circe.generic.semiauto._
  private val derivedDecoder = deriveDecoder[PartWithOpts]
  private def decodeFromString[T: Decoder]: Decoder[PartWithOpts] = (c: HCursor) =>
    for { part <- c.as[String] } yield PartWithOpts(part)
  implicit val decoder: Decoder[PartWithOpts] = decodeFromString or derivedDecoder
}
case class WFC(parts: Seq[PartWithOpts], defs: Map[String, PaletteDef]) extends RoomGenAlgorithm {
  sealed trait AdjacencyType {
    def rotated: AdjacencyType = this
    def flipped: AdjacencyType = this

    def matchesEdge: Boolean = false
    def matchesEdgeDoor: Boolean = false
    def requiresEdgeDoor: Boolean = false
  }
  case class Matching(c: Char) extends AdjacencyType {
    override def matchesEdge: Boolean = false
  }
  case object Edge extends AdjacencyType {
    override def matchesEdge: Boolean = true
  }
  case object EdgeDoor extends AdjacencyType {
    override def matchesEdge: Boolean = true
    override def matchesEdgeDoor: Boolean = true
    override def requiresEdgeDoor: Boolean = true
  }
  case class Internal(s: String, r: Int = 0, f: Boolean = false) extends AdjacencyType {
    override def rotated: AdjacencyType = copy(r = (r + 1) % 4)
    override def flipped: AdjacencyType = copy(f = !f)
  }
  case object Any extends AdjacencyType {
    override def matchesEdge: Boolean = true
    override def matchesEdgeDoor: Boolean = true
  }

  parts.view.flatMap(_.part.toCharArray).to(Set).filterNot(c => c == 'x' || c == '*' || c == '+' || c == '_' || c == '\n') foreach { c =>
    assert(defs.contains(c.toString), s"Expected defs to contain '$c'")
  }

  case class Tile(
    value: Char,
    left: AdjacencyType,
    right: AdjacencyType,
    up: AdjacencyType,
    down: AdjacencyType,
    partId: Int = -1,
    weight: Double = 1,
    isFirst: Boolean = false,
  ) {

    override def toString: String = {
      s"T[$value, l=$left, r=$right, u=$up, d=$down p=$partId]"
    }
    def rotated: Tile = copy(
      left = up.rotated,
      up = right.rotated,
      right = down.rotated,
      down = left.rotated,
    )
    def flippedX: Tile = copy(
      left = right.flipped,
      right = left.flipped,
    )

    def flippedY: Tile = copy(
      up = down.flipped,
      down = up.flipped,
    )
  }

  private def tilesFromPart(part: PartWithOpts, i: Int) = {
    val lines = part.part.split("\n")
    // for now, only support rectangles
    val width = lines.head.length
    val height = lines.length
    assert(lines.forall(l => l.length == width), "WFC parts must be rectangular for now")
    val grid = new Grid(width + 2, height + 2)('_')
    for (y <- 0 until height; x <- 0 until width)
      grid(x + 1, y + 1) = lines(y)(x)
    def edgeCharToAdj(c: Char): AdjacencyType = c match {
      case '*' => Any
      case 'x' => Edge
      case '+' => EdgeDoor
      case _ => Matching(c)
    }
    def isEdge(x: Int, y: Int): Boolean = {
      grid(x - 1, y) == '_' || grid(x + 1, y) == '_' || grid(x, y - 1) == '_' || grid(x, y + 1) == '_'
    }
    var first = true
    val tiles = (for (y <- 0 until height; x <- 0 until width) yield {
      val tx = x + 1
      val ty = y + 1
      val value = grid(tx, ty)
      if (!isEdge(tx, ty))
        Some(
          Tile(
            partId = i,
            isFirst = { val wasFirst = first; first = false; wasFirst },
            weight = part.weight.getOrElse(1),
            value = value,
            left = if (isEdge(tx - 1, ty)) edgeCharToAdj(grid(tx - 1, ty)) else Internal(s"Part $i ${x - 1},$y h"),
            right = if (isEdge(tx + 1, ty)) edgeCharToAdj(grid(tx + 1, ty)) else Internal(s"Part $i $x,$y h"),
            up = if (isEdge(tx, ty - 1)) edgeCharToAdj(grid(tx, ty - 1)) else Internal(s"Part $i $x,${y - 1} v"),
            down = if (isEdge(tx, ty + 1)) edgeCharToAdj(grid(tx, ty + 1)) else Internal(s"Part $i $x,$y v"),
          )
        )
      else None
    }).flatten
    val doRotate = part.rotate.getOrElse(true)
    val doFlip = part.flip.getOrElse(true)
    val rotated =
      if (doRotate)
        tiles.map(_.rotated) ++ tiles.map(_.rotated.rotated) ++ tiles.map(_.rotated.rotated.rotated)
      else
        Seq.empty
    val flipped =
      if (doFlip)
        tiles.map(_.flippedX) ++ (if (!doRotate) tiles.map(_.flippedY) else Seq.empty)
      else Seq.empty
    val rotatedAndFlipped =
      if (doRotate && doFlip)
        flipped.map(_.rotated) ++ flipped.map(_.rotated.rotated) ++ flipped.map(_.rotated.rotated.rotated)
      else Seq.empty
    tiles ++ rotated ++ flipped ++ rotatedAndFlipped
  }

  private val partTiles: Seq[Tile] = for {
    (part, i) <- parts.zipWithIndex
    tile <- tilesFromPart(part, i)
  } yield tile

  private val missingTiles = {
    partTiles.flatMap { t =>
      // if there's no tile that could possibly match |t| to the left...
      (t.left match {
        case Matching(c) if !partTiles.exists(matchesHorizontal(_, t)) =>
          // generate a new tile that will.
          println(s"Tile $t was not matchable on the left")
          Seq(Tile(c, Any, right = Matching(t.value), Any, Any))
        case _ => Seq.empty
      }) ++ (t.right match {
        case Matching(c) if !partTiles.exists(matchesHorizontal(t, _)) =>
          Seq(Tile(c, left = Matching(t.value), Any, Any, Any))
        case _ => Seq.empty
      }) ++ (t.up match {
        case Matching(c) if !partTiles.exists(matchesVertical(_, t)) =>
          Seq(Tile(c, Any, Any, Any, down = Matching(t.value)))
        case _ => Seq.empty
      }) ++ (t.down match {
        case Matching(c) if !partTiles.exists(matchesVertical(t, _)) =>
          Seq(Tile(c, Any, Any, up = Matching(t.value), Any))
        case _ => Seq.empty
      })
    }
  }.distinctBy(t => (t.left, t.right, t.up, t.down, t.value))

  private val allTilesDup: Seq[Tile] = (partTiles ++ missingTiles).to(IndexedSeq)
  private val (allTiles, tileWeights) =
    allTilesDup
      .groupBy(t => (t.value, t.left, t.right, t.up, t.down))
      .view.values.map { v => (v.head, v.size * v.head.weight) }
      .toIndexedSeq
      .unzip

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

  allTiles.foreach(t => assert(t.left == Edge || t.left == EdgeDoor || allTiles.exists(k => matchesHorizontal(k, t)), s"No tile matches $t on the left"))
  allTiles.foreach(t => assert(t.right == Edge || t.right == EdgeDoor || allTiles.exists(k => matchesHorizontal(t, k)), s"No tile matches $t on the right"))
  allTiles.foreach(t => assert(t.up == Edge || t.up == EdgeDoor || allTiles.exists(k => matchesVertical(k, t)), s"No tile matches $t on the up"))
  allTiles.foreach(t => assert(t.down == Edge || t.down == EdgeDoor || allTiles.exists(k => matchesVertical(t, k)), s"No tile matches $t on the down"))

  private def gts(isDoorEdge: (Int, Int) => Boolean): GraphTileSet = new GraphTileSet {
    override def size: Int = allTiles.size

    /** true if |left| can be placed to the left of |right| */
    override def allowedHorizontal(left: Int, right: Int): Boolean = {
      if (left < 0) return allTiles(right).left.matchesEdge
      if (right < 0) return allTiles(left).right.matchesEdge
      matchesHorizontal(allTiles(left), allTiles(right))
    }

    /** true if |top| can be placed above |bottom| */
    override def allowedVertical(top: Int, bottom: Int): Boolean = {
      if (top < 0) return allTiles(bottom).up.matchesEdge
      if (bottom < 0) return allTiles(top).down.matchesEdge
      matchesVertical(allTiles(top), allTiles(bottom))
    }

    /** true if the player can navigate from |left| to |right| */
    override def connectedHorizontal(left: Int, right: Int): Boolean = true

    /** true if the player can navigate from |top| to |bottom| */
    override def connectedVertical(top: Int, bottom: Int): Boolean = true

    override def allowedAt(x: Int, y: Int, t: Int): Boolean = {
      val tile = allTiles(t)
      (if (isDoorEdge(x - 1, y)) {
        tile.left.matchesEdgeDoor
      } else {
        !tile.left.requiresEdgeDoor
      }) && (if (isDoorEdge(x + 1, y)) {
        tile.right.matchesEdgeDoor
      } else {
        !tile.right.requiresEdgeDoor
      }) && (if (isDoorEdge(x, y - 1)) {
        tile.up.matchesEdgeDoor
      } else {
        !tile.up.requiresEdgeDoor
      }) && (if (isDoorEdge(x, y + 1)) {
        tile.down.matchesEdgeDoor
      } else {
        !tile.down.requiresEdgeDoor
      })
    }

    override def countConstraints: Iterable[WaveFunctionCollapse.CountConstraint] = {
      for (i <- parts.indices; if parts(i).min.nonEmpty || parts(i).max.nonEmpty) yield {
        val representatives = allTiles.indices.filter { ti => val t = allTiles(ti); t.isFirst && t.partId == i }
        val lb = parts(i).min.getOrElse(0)
        val ub = parts(i).max.getOrElse(Integer.MAX_VALUE)
        WaveFunctionCollapse.CountConstraint(representatives, lb, ub)
      }
    }

    override def weight(t: Int): Double = tileWeights(t)
  }

  def generateChars(width: Int, height: Int, isDoorEdge: (Int, Int) => Boolean, watcher: ((Int, Int) => Tile, Boolean) => Unit = null)(implicit r: Random): Grid[Tile] = {
    val decisionCb = if (watcher != null) {
      (value: (Int, Int) => Int, isContradiction: Boolean) => {
        val valueAsTile = (x: Int, y: Int) => {
          val v = value(x, y); if (v >= 0) allTiles(v) else null
        }
        watcher(valueAsTile, isContradiction)
      }
    } else null
    WaveFunctionCollapse.graphSolve(gts(isDoorEdge), width, height, r, decisionCallback = decisionCb) match {
      case Some(result) =>
        val grid = new Grid[Tile](width, height)(null)
        for (y <- 0 until height; x <- 0 until width) {
          val tile = allTiles(result(x)(y))
          grid(x, y) = tile
        }
        grid
      case None =>
        throw new RuntimeException(s"Failed to generate $width x $height room")
    }
  }

  def fill(state: GameState, levelId: LevelId, xmin: Int, ymin: Int, tileGrid: Grid[Tile]): Unit = {
    for (y <- 0 until tileGrid.height; x <- 0 until tileGrid.width) {
      val tile = tileGrid(x, y)
      val paletteDef = defs(tile.value.toString)
      val terrain = paletteDef.terrain
      val tx = xmin + x
      val ty = ymin + y
      state.levels(levelId).terrain(tx, ty) = state.data.terrain(terrain.getOrElse("floor"))
      paletteDef.items.foreach { table =>
        state.sampleItemWithExtras(table) foreach { itemWithExtras =>
          val item = state.data.items(itemWithExtras.item).generateItem()
          state.items.put(item, OnFloor(Location(levelId, tx, ty)))
          for (spec <- itemWithExtras.connect) {
            item.behaviors.find(_.isInstanceOf[HasPorts]) match {
              case Some(bhvr: HasPorts) =>
                bhvr.ports.find(_.name == spec.port) match {
                  case Some(portSpec) =>
                    val cableType = portSpec.`type`;
                    (cableType match {
                      case "power-in" | "power-out" =>
                        state.levels(levelId).powerCables
                      case "data-in" | "data-out" =>
                        state.levels(levelId).dataCables
                      case "fluid-in" | "fluid-out" =>
                        state.levels(levelId).fluidCables
                    })(tx, ty) |= spec.layer
                    bhvr.connections += (spec.port -> new LayerSet(spec.layer))
                  case None =>
                    println(s"Warning: ${item.kind.name} has no port named ${spec.port}")
                }
              case _ =>
                println(s"Warning: ${item.kind.name} does not have ports")
            }
          }
        }
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
    def isDoorEdge(x: Int, y: Int): Boolean = {
      (x < 0 || x >= width || y < 0 || y >= height) &&
        state.terrain(Location(levelId, xmin + x, ymin + y)).exists(_.name == "floor")
    }
    val tileGrid = generateChars(width, height, isDoorEdge)
    fill(state, levelId, xmin, ymin, tileGrid)
  }
}

case class Static(
  layout: String,
  defs: Map[String, PaletteDef],
  powerCables: String,
  fluidCables: String,
  dataCables: String,
) extends RoomGenAlgorithm {
  private val lines = layout.split("\n")
  private val powerCableLines = powerCables.split("\n")
  private val fluidCableLines = fluidCables.split("\n")
  private val dataCableLines = dataCables.split("\n")
  val width: Int = lines.map(_.length).max
  val height: Int = lines.length
  assert(powerCableLines.length <= height)
  assert(powerCableLines.map(_.length).max <= width)
  override def generate(state: GameState, levelId: LevelId, cells: Seq[(Int, Int)])(implicit r: Random): Unit = {
    val tileGrid = new Grid[Char](width, height)(' ')
    for ((line, y) <- lines.zipWithIndex; (char, x) <- line.zipWithIndex) {
      tileGrid(x, y) = char
    }
    val cableGrid = new Grid[(Char, Char, Char)](width, height)(('0','0','0'))
    for ((line, y) <- powerCableLines.zipWithIndex; (char, x) <- line.zipWithIndex) {
      cableGrid(x, y) = cableGrid(x, y).copy(_1 = char)
    }
    for ((line, y) <- fluidCableLines.zipWithIndex; (char, x) <- line.zipWithIndex) {
      cableGrid(x, y) = cableGrid(x, y).copy(_2 = char)
    }
    for ((line, y) <- dataCableLines.zipWithIndex; (char, x) <- line.zipWithIndex) {
      cableGrid(x, y) = cableGrid(x, y).copy(_3 = char)
    }
    fill(state, levelId, cells.map(_._1).min, cells.map(_._2).min, tileGrid, cableGrid)
  }

  def fill(state: GameState, levelId: LevelId, xmin: Int, ymin: Int, tileGrid: Grid[Char], cableGrid: Grid[(Char, Char, Char)]): Unit = {
    for (y <- 0 until tileGrid.height; x <- 0 until tileGrid.width) {
      val tile = tileGrid(x, y)
      val paletteDef = tile.toString match {
        case " " => defs.getOrElse(" ", PaletteDef())
        case other => defs.getOrElse(other, throw new Exception(s"No palette def for '$other'"))
      }
      val cableTile = cableGrid(x, y)
      val powerCableDef = cableTile._1 match {
        case x if x >= '0' && x <= '9' => x - '0'
        case x if x >= 'a' && x <= 'v' => x - 'a' + 10
        case _ => 0
      }
      val fluidCableDef = cableTile._2 match {
        case x if x >= '0' && x <= '9' => x - '0'
        case x if x >= 'a' && x <= 'v' => x - 'a' + 10
        case _ => 0
      }
      val dataCableDef = cableTile._3 match {
        case x if x >= '0' && x <= '9' => x - '0'
        case x if x >= 'a' && x <= 'v' => x - 'a' + 10
        case _ => 0
      }
      val terrain = paletteDef.terrain
      val tx = xmin + x
      val ty = ymin + y
      state.levels(levelId).terrain(tx, ty) = state.data.terrain(terrain.getOrElse("empty space"))
      state.levels(levelId).powerCables(tx, ty) = powerCableDef
      state.levels(levelId).dataCables(tx, ty) = dataCableDef
      state.levels(levelId).fluidCables(tx, ty) = fluidCableDef
      paletteDef.items.foreach { table =>
        state.sampleItemWithExtras(table) foreach { itemWithExtras =>
          val item = state.data.items.get(itemWithExtras.item) match {
            case Some(v) => v.generateItem()
            case None => throw new RuntimeException(s"no such item: ${itemWithExtras.item}")
          }
          for (containedItem <- state.sampleItem(itemWithExtras.contents))
            state.items.put(containedItem, Inside(item))
          state.items.put(item, OnFloor(Location(levelId, tx, ty)))
          for (spec <- itemWithExtras.connect) {
            item.behaviors.find(_.isInstanceOf[HasPorts]) match {
              case Some(bhvr: HasPorts) =>
                bhvr.ports.find(_.name == spec.port) match {
                  case Some(portSpec) =>
                    val cableType = portSpec.`type`;
                    (cableType match {
                      case "power-in" | "power-out" =>
                        state.levels(levelId).powerCables
                      case "data-in" | "data-out" =>
                        state.levels(levelId).dataCables
                      case "fluid-in" | "fluid-out" =>
                        state.levels(levelId).fluidCables
                    })(tx, ty) |= spec.layer
                    bhvr.connections += (spec.port -> new LayerSet(spec.layer))
                  case None =>
                    println(s"Warning: ${item.kind.name} has no port named ${spec.port}")
                }
              case _ =>
                println(s"Warning: ${item.kind.name} does not have ports")
            }
          }
        }
      }
    }
  }
}

object RoomGenAlgorithm {
  import cats.syntax.functor._
  import io.circe.generic.extras.Configuration
  import io.circe.generic.extras.auto._
  import io.circe.{Decoder, Encoder}
  implicit private val configuration: Configuration = Configuration.default.withDefaults.withDiscriminator("type")

  val decoders: Map[String, Decoder[RoomGenAlgorithm]] = Map(
    "Furnish" -> Decoder[Furnish].widen,
    "WFC" -> Decoder[WFC].widen,
    "Static" -> Decoder[Static].widen,
  )

  implicit val encodeBehavior: Encoder[RoomGenAlgorithm] = (b: RoomGenAlgorithm) => Json.fromJsonObject(JsonObject.singleton(b.getClass.getSimpleName, b match {
    case b: Furnish => Encoder[Furnish].apply(b)
    case b: WFC => Encoder[WFC].apply(b)
    case b: Static => Encoder[Static].apply(b)
  }))
}
