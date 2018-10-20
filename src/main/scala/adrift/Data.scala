package adrift

import java.nio.file.{FileSystems, Files, Path, PathMatcher}
import java.util.stream.Collectors

import adrift.items.{HandleOp, ItemKind}
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.{Json, JsonObject, yaml}

import scala.collection.JavaConverters._
import scala.collection.mutable

sealed trait YamlObject
object YamlObject {
  case class ItemKind(
    name: String,
    description: String,
    parts: Seq[ItemPart] = Seq.empty,
  ) extends YamlObject

  case class ItemPart(
    `type`: String,
    disassembled_with: String = "hand",
    count: Int = 1
  )

  case class Operation(id: String) extends YamlObject

  case class RoomDef(
    name: String,
    rotatable: Boolean = false,
    flippable: Boolean = false,
    layout: String,
    defs: Map[String, JsonObject] = Map.empty,
    default_terrain: String = "floor",
    items: Seq[JsonObject] = Seq.empty,
    connections: Map[String, String] = Map.empty,
  )
}

case class Data(
  items: Map[String, ItemKind],
  rooms: Map[String, YamlObject.RoomDef],
  terrain: Map[String, Terrain],
)

object Data {
  implicit private val configuration: Configuration = Configuration.default.withDefaults
  private val matcher = FileSystems.getDefault.getPathMatcher("glob:**.yml")

  def parse(dir: Path): Data = {
    val ymls: Map[String, Seq[Json]] = Files.walk(dir)
      .filter(matcher.matches _)
      .flatMap[Json] { f =>
        val xs = yaml.parser.parse(Files.newBufferedReader(f)).fold(
          ex => throw new RuntimeException(s"Failed to parse $f", ex),
          identity
        )
        java.util.Arrays.stream(xs.asArray.get.toArray)
      }
      .collect(Collectors.toList[Json]).asScala
      .groupBy(obj => obj.hcursor.get[String]("type").right.getOrElse { throw new RuntimeException(s"Failed to parse (missing 'type' key): $obj") })

    val items: Map[String, ItemKind] = parseItems(ymls("item"))

    val rooms = ymls("room")
      .map(obj => obj.as[YamlObject.RoomDef]
        .fold(ex => throw new RuntimeException(s"Failed to parse room: $obj", ex), identity))
      .groupBy(_.name).map {
        case (k, v) => assert(v.length == 1); k -> v.head
      }

    val terrain: Map[String, Terrain] = ymls("terrain")
      .map(obj => obj.as[Terrain]
        .fold(ex => throw new RuntimeException(s"Failed to parse terrain: $obj", ex), identity))
      .groupBy(_.name).map {
        case (k, v) => assert(v.length == 1); k -> v.head
      }

    Data(
      items,
      rooms,
      terrain
    )
  }

  private def parseItems(items: Seq[Json]): Map[String, ItemKind] = {
    val itemDefs = items.map(itemObj =>
      itemObj.as[YamlObject.ItemKind].fold(
        ex => throw new RuntimeException(itemObj.toString(), ex),
        identity
      )
    )
    val itemsById = itemDefs.groupBy(_.name).map {
      case (k, v) =>
        assert(v.size == 1, s"more than one item with the name '$k'")
        k -> v.head
    }
    val lazyMap = mutable.Map.empty[String, ItemKind]

    def itemForId(id: String): ItemKind = {
      lazyMap.getOrElseUpdate(
        id, {
          val i = itemsById(id)
          ItemKind(
            i.name,
            i.description,
            i.parts.map { p =>
              ((itemForId(p.`type`), p.count), HandleOp() /* TODO */ )
            }
          )
        }
      )
    }

    itemsById.map { case (k, _) => k -> itemForId(k) }
  }
}

