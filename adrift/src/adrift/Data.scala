package adrift

import java.nio.file.{FileSystems, Files, Path}
import java.util.stream.Collectors

import adrift.Population.Table
import adrift.items.{Behavior, ItemKind, ItemOperation, ItemPart}
import adrift.worldgen.RoomGen
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.generic.extras.semiauto._
import io.circe._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.matching.Regex

object YamlObject {
  case class ItemKind(
    name: String,
    description: String,
    parts: Seq[ItemPart],
    provides: Seq[String] = Seq.empty,
    display: String,
    behavior: Seq[JsonObject] = Seq.empty,
  )

  case class ItemPart(
    `type`: String,
    disassembled_with: String = "HANDLING",
    count: Int = 1
  )

  case class ItemGroup(
    name: String,
    choose: Table[String]
  )

  case class RoomDef(
    name: String,
    rotatable: Boolean = false,
    flippable: Boolean = false,
    layout: String,
    defs: Map[String, JsonObject] = Map.empty,
    gen: Option[String] = None,
    default_terrain: String = "floor",
    items: Seq[JsonObject] = Seq.empty,
    connections: Map[String, String] = Map.empty,
  ) {
    for (line <- layout.linesIterator; char <- line; if !char.isSpaceChar)
      assert(defs.contains(char.toString), s"'$char' not present in defs of room '$name'")
  }

  case class RoomGen(
    name: String,
    algorithm: String,
    options: JsonObject
  )

  case class SectorRoom(
    room: String,
  )

  case class SectorDef(
    name: String,
    rooms: Seq[SectorRoom]
  )
}

case class Data(
  items: Map[String, ItemKind],
  itemGroups: Map[String, YamlObject.ItemGroup],
  rooms: Map[String, YamlObject.RoomDef],
  roomgens: Map[String, RoomGen],
  terrain: Map[String, Terrain],
  sectors: Map[String, YamlObject.SectorDef],
  display: DisplayData,
)

case class Color(r: Float, g: Float, b: Float, a: Float) {
  def darken(pct: Float): Color = Color(r*pct, g*pct, b*pct, a)
}
object Color {
  val hexColor: Regex = raw"#([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})?".r
  implicit val decoder: Decoder[Color] = { (h: HCursor) =>
    h.as[String].flatMap {
      case hexColor(r, g, b, null) =>
        Right(Color(
          Integer.parseUnsignedInt(r, 16) / 255f,
          Integer.parseUnsignedInt(g, 16) / 255f,
          Integer.parseUnsignedInt(b, 16) / 255f,
          1f
        ))
      case hexColor(r, g, b, a) =>
        Right(Color(
          Integer.parseUnsignedInt(r, 16) / 255f,
          Integer.parseUnsignedInt(g, 16) / 255f,
          Integer.parseUnsignedInt(b, 16) / 255f,
          Integer.parseUnsignedInt(a, 16) / 255f
        ))
      case other => Left(DecodingFailure(s"Failed to parse color: '$other'", h.history))
    }
  }
  val White = Color(1f, 1f, 1f, 1f)
  val Black = Color(0f, 0f, 0f, 1f)
}

case class DisplayProps(
  char: Char = '.',
  fg: String = "default",
  bg: String = "default",
  layer: Int = 0,
)
case class DisplayData(
  palette: Map[String, Color],
  default: DisplayProps,
  categories: Map[String, DisplayProps]
) {
  def getDisplay(category: String): (Char, Color, Color, Int) = {
    val DisplayProps(char, fgName, bgName, layer) = categories(category)
    val fg = palette(if (fgName == "default") default.fg else fgName)
    val bg = palette(if (bgName == "default") default.bg else bgName)
    (char, fg, bg, layer)
  }
}

object Data {
  implicit private val configuration: Configuration = Configuration.default.withDefaults
  private val matcher = FileSystems.getDefault.getPathMatcher("glob:**.{yml,yaml}")

  implicit val itemKindDecoder: Decoder[YamlObject.ItemKind] = deriveDecoder[YamlObject.ItemKind].prepare {
    _.withFocus(j => j.mapObject(o => { if (!o.contains("parts")) o.add("parts", Json.arr()) else o }))
  }

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
      .collect(Collectors.toList[Json]).asScala.toSeq
      .groupBy(obj => obj.hcursor.get[String]("type").right.getOrElse { throw new RuntimeException(s"Failed to parse (missing 'type' key): $obj") })

    val operations: Map[String, ItemOperation] = ymls("operation")
      .map(obj => obj.as[ItemOperation]
        .fold(ex => throw new RuntimeException(s"Failed to parse operation: $obj", ex), identity))
      .groupBy(_.id).map { case (k, v) => assert(v.length == 1); k -> v.head }

    val items: Map[String, ItemKind] = parseItems(ymls("item"), operations)

    val itemGroups: Map[String, YamlObject.ItemGroup] = ymls("item_group")
      .map(obj => obj.as[YamlObject.ItemGroup]
        .fold(ex => throw new RuntimeException(s"Failed to parse item_group: $obj", ex), identity))
      .groupBy(_.name).map {
        case (k, v) => assert(v.length == 1, s"More than one item group with name $k"); k -> v.head
      }

    val rooms: Map[String, YamlObject.RoomDef] = ymls("room")
      .map(obj => obj.as[YamlObject.RoomDef]
        .fold(ex => throw new RuntimeException(s"Failed to parse room: $obj", ex), identity))
      .groupBy(_.name).map {
        case (k, v) => assert(v.length == 1, s"More than one room with name $k"); k -> v.head
      }

    val roomgens: Map[String, RoomGen] = ymls("roomgen")
      .map(obj => obj.as[YamlObject.RoomGen]
        .fold(ex => throw new RuntimeException(s"Failed to parse roomgen: $obj", ex), identity))
      .groupBy(_.name).map { case (k: String, v: Seq[YamlObject.RoomGen]) =>
        assert(v.length == 1, s"More than one roomgen with name $k"); k -> v.head
      }
      .map { case (k: String, v: YamlObject.RoomGen) =>
        k -> RoomGen.decoders(v.algorithm).decodeJson(Json.fromJsonObject(v.options)).fold(throw _, identity)
      }

    val terrain: Map[String, Terrain] = ymls("terrain")
      .map(obj => obj.as[Terrain]
        .fold(ex => throw new RuntimeException(s"Failed to parse terrain: $obj", ex), identity))
      .groupBy(_.name).map {
        case (k, v) => assert(v.length == 1, s"More than one terrain with name $k"); k -> v.head
      }

    val sectors: Map[String, YamlObject.SectorDef] = ymls("sector")
      .map(obj => obj.as[YamlObject.SectorDef]
        .fold(ex => throw new RuntimeException(s"Failed to parse sector: $obj", ex), identity))
      .groupBy(_.name).map {
        case (k, v) => assert(v.length == 1, s"More than one sector with name $k"); k -> v.head
      }
    for ((name, sd) <- sectors; r <- sd.rooms)
      assert(rooms.contains(r.room), s"Sector '$name' references room '${r.room}', but that room doesn't exist")

    val display: DisplayData = {
      val displays = ymls("display")
      assert(displays.size == 1, "Must have exactly one display object")
      displays.head.as[DisplayData].fold(throw _, identity)
    }

    Data(
      items,
      itemGroups,
      rooms,
      roomgens,
      terrain,
      sectors,
      display
    )
  }

  private def parseItems(items: Seq[Json], operations: Map[String, ItemOperation]): Map[String, ItemKind] = {
    val itemDefs = items.map(itemObj =>
      itemObj.as[YamlObject.ItemKind].fold(
        ex => throw new RuntimeException(itemObj.toString(), ex),
        identity
      )
    )
    val itemsById: Map[String, YamlObject.ItemKind] = itemDefs.groupBy(_.name).map {
      case (k, v) =>
        assert(v.size == 1, s"more than one item with the name '$k'")
        k -> v.head
    }
    val lazyMap = mutable.Map.empty[String, ItemKind]

    def itemForId(id: String): ItemKind = {
      lazyMap.getOrElseUpdate(
        id, {
          val i = itemsById(id)
          val behaviorGenerators = i.behavior.map { obj =>
            val behaviorType = obj.keys.head
            if (!Behavior.decoders.contains(behaviorType))
              throw new RuntimeException(s"Behavior $behaviorType isn't decodable")
            val args = obj(behaviorType).get
            () => Behavior.decoders(behaviorType).decodeJson(args).fold(throw _, identity)
          }
          ItemKind(
            i.name,
            i.description,
            i.parts.map { p =>
              val operation = operations.getOrElse(p.disassembled_with, throw new RuntimeException(s"item '${i.name}' specified an operation '${p.disassembled_with}' which doesn't seem to exist"))
              ItemPart(itemForId(p.`type`), p.count, operation)
            },
            display = i.display,
            behaviors = behaviorGenerators
          )
        }
      )
    }

    itemsById.map { case (k, _) => k -> itemForId(k) }
  }
}

