package adrift.worldgen

import adrift.{GameState, OnFloor}
import adrift.Population.Table
import io.circe.{Json, JsonObject}
import adrift.RandomImplicits._

import scala.util.Random

trait RoomGen {
  def generate(state: GameState, cells: Seq[(Int, Int)])(implicit r: Random): Unit
}

case class FurnishItem(`type`: Table[String], wall_adjacent: Boolean = false, nearby: Option[Table[String]])
case class Furnish(items: Table[FurnishItem]) extends RoomGen {
  override def generate(state: GameState, cells: Seq[(Int, Int)])(implicit r: Random): Unit = {
    val cellSet = cells.toSet
    def isWallAdjacent(p: (Int, Int)): Boolean = {
      val (x, y) = p
      !cellSet(x-1, y) || !cellSet(x+1, y) || !cellSet(x, y-1) || !cellSet(x, y+1)
    }
    def isEmpty(p: (Int, Int)): Boolean = state.items.lookup(OnFloor(p._1, p._2)).isEmpty
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
        for (i <- items) state.items.put(i, OnFloor(loc._1, loc._2))
        for (nearby <- item.nearby; nearbyLoc <- chooseNearbyLocation(loc)) {
          val items = state.sampleItem(nearby)
          for (i <- items) state.items.put(i, OnFloor(nearbyLoc._1, nearbyLoc._2))
        }
      }
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
  )

  implicit val encodeBehavior: Encoder[RoomGen] = (b: RoomGen) => Json.fromJsonObject(JsonObject.singleton(b.getClass.getSimpleName, b match {
    case b: Furnish => Encoder[Furnish].apply(b)
  }))
}
