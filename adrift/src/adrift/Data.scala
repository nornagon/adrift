package adrift

import java.nio.file.{FileSystems, Files, Path}
import java.util.stream.Collectors
import adrift.Population.Table
import adrift.YamlObject.ItemWithExtras
import adrift.items.{Behavior, ItemKind, ItemOperation, ItemPart}
import adrift.worldgen.{RoomGen, RoomGenAlgorithm, WFC}
import io.circe._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.generic.extras.semiauto._

import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.util.matching.Regex

object YamlObject {
  case class ItemKind(
    name: String,
    description: String,
    parts: Seq[ItemPart],
    provides: Seq[String] = Seq.empty,
    display: String,
    volume: Volume = Volume(Int.MaxValue),
    behavior: Seq[JsonObject] = Seq.empty,
  )

  case class ItemPart(
    `type`: String,
    disassembled_with: String = "HANDLING",
    count: Int = 1
  )

  case class ItemGroup[T](
    name: String,
    choose: Table[T]
  )

  case class ConnectSpec(port: String, layer: Int)
  case class ItemWithExtras(
    item: String,
    contents: Table[ItemWithExtras] = Table.empty,
    connect: Seq[ConnectSpec] = Seq.empty
  )
  object ItemWithExtras {
    implicit private val configuration: Configuration = Configuration.default.withDefaults
    implicit val itemWithExtrasDecoder: Decoder[ItemWithExtras] = { (c: HCursor) =>
      c.as[String].map(ItemWithExtras(_))
        .orElse(deriveConfiguredDecoder[ItemWithExtras].apply(c))
    }
  }

  case class RoomGen(
    name: String,
    algorithm: String,
    minArea: Int = 0,
    maxArea: Int = Integer.MAX_VALUE,
    options: JsonObject
  )
}

case class Data(
  items: Map[String, ItemKind],
  itemGroups: Map[String, YamlObject.ItemGroup[YamlObject.ItemWithExtras]],
  roomgens: Map[String, RoomGen],
  terrain: Map[String, Terrain],
  display: DisplayData,
)

case class Color(r: Float, g: Float, b: Float, a: Float) {
  def darken(pct: Float): Color = Color(r*pct, g*pct, b*pct, a)
  // TODO: LCH?
  def lerp(other: Color, t: Float): Color = Color(
    r * (1 - t) + other.r * t,
    g * (1 - t) + other.g * t,
    b * (1 - t) + other.b * t,
    a * (1 - t) + other.a * t,
  )
}
object Color {
  val hexColor: Regex = raw"#([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})?".r
  implicit val decoder: Decoder[Color] = { (h: HCursor) =>
    h.as[String].flatMap { s =>
      fromHex(s) match {
        case Some(c) => Right(c)
        case None => Left(DecodingFailure(s"Failed to parse color: '$s'", h.history))
      }
    }
  }
  def fromBytes(r: Int, g: Int, b: Int, a: Int = 255): Color = Color(r / 255f, g / 255f, b / 255f, a / 255f)
  def fromHex(hexString: String): Option[Color] = hexString match {
    case hexColor(r, g, b, null) =>
      Some(Color.fromBytes(
        Integer.parseUnsignedInt(r, 16),
        Integer.parseUnsignedInt(g, 16),
        Integer.parseUnsignedInt(b, 16),
      ))
    case hexColor(r, g, b, a) =>
      Some(Color.fromBytes(
        Integer.parseUnsignedInt(r, 16),
        Integer.parseUnsignedInt(g, 16),
        Integer.parseUnsignedInt(b, 16),
        Integer.parseUnsignedInt(a, 16)
      ))
    case _ => None
  }
  val White: Color = Color(1f, 1f, 1f, 1f)
  val Black: Color = Color(0f, 0f, 0f, 1f)
  val Transparent: Color = Color(0f, 0f, 0f, 0f)
}

case class Gradient(colors: Seq[Color]) {
  require(colors.nonEmpty)
  def sample(t: Float): Color = {
    if (colors.size <= 1) return colors.head
    if (t >= 1f) return colors.last
    if (t <= 0f) return colors.head
    val tScaled = t * (colors.size - 1)
    val tClamped = math.max(0, math.min(colors.size - 1, tScaled))
    val aIndex = math.min(colors.size - 2, tClamped.floor.toInt)
    val bIndex = aIndex + 1
    val a = colors(aIndex)
    val b = colors(bIndex)
    a.lerp(b, tClamped % 1f)
  }
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

case class Volume(milliliters: Int) extends AnyVal with Ordered[Volume] {
  def +(other: Volume): Volume = Volume(milliliters + other.milliliters)
  def -(other: Volume): Volume = Volume(milliliters - other.milliliters)

  override def compare(that: Volume): Int = implicitly[Ordered[Int]](milliliters).compare(that.milliliters)
}
object Volume {
  private val milliliters = """(?i)\s*(\d*\.?\d+)\s*ml""".r
  private val liters = """(?i)\s*(\d*\.?\d+)\s*l""".r
  implicit val decoder: Decoder[Volume] = { (h: HCursor) =>
    h.as[String].flatMap {
      case milliliters(c) => Right(Volume(c.toFloat.round))
      case liters(c) => Right(Volume((c.toFloat * 1000).round))
      case other => Left(DecodingFailure(s"Failed to parse volume: '$other'", h.history))
    }
  }
}

object Data {
  implicit private val configuration: Configuration = Configuration.default.withDefaults
  private val matcher = FileSystems.getDefault.getPathMatcher("glob:**.{yml,yaml}")

  implicit val itemKindDecoder: Decoder[YamlObject.ItemKind] = deriveConfiguredDecoder[YamlObject.ItemKind].prepare {
    _.withFocus(j => j.mapObject(o => { if (!o.contains("parts")) o.add("parts", Json.arr()) else o }))
  }
  private def parseError(`type`: String, obj: Json, ex: DecodingFailure): Nothing = {
    throw new RuntimeException(s"Failed to parse ${`type`} at ${CursorOp.opsToPath(ex.history)}: ${ex.message}\nObject: $obj")
  }
  // Like obj.as[A] but it unwraps any errors and produces a nice(-ish) error message if the decoding fails.
  private def parse[A](obj: Json)(implicit d: Decoder[A]): A =
    obj.as[A].fold(
      ex => {
        val objType = obj.asObject.flatMap(_("type")).flatMap(_.asString).getOrElse("unknown")
        parseError(objType, obj, ex)
      },
      identity
    )

  def parse(dir: Path): Data = {
    val ymls: Map[String, Seq[Json]] = Files.walk(dir)
      .filter(matcher.matches _)
      .flatMap[Json] { f =>
        val xs = yaml.parser.parse(Files.newBufferedReader(f)).fold(
          ex => throw new RuntimeException(s"Failed to parse $f", ex),
          identity
        )
        if (xs == Json.False)
          // circe parses the empty document as 'false'. Ignore it.
          java.util.stream.Stream.empty()
        else
          xs.asArray.map(arr => java.util.Arrays.stream(arr.toArray))
            .getOrElse(throw new RuntimeException(s"Expected $f to contain an array, but found ${xs.name}"))
      }
      .collect(Collectors.toList[Json]).asScala.toSeq
      .groupBy(obj => obj.hcursor.get[String]("type").getOrElse { throw new RuntimeException(s"Failed to parse (missing 'type' key): $obj") })

    val operations: Map[String, ItemOperation] = ymls("operation")
      .map(parse[ItemOperation])
      .groupBy(_.id).map { case (k, v) => assert(v.length == 1); k -> v.head }

    val items: Map[String, ItemKind] = parseItems(ymls("item"), operations)
    println(s"${items.size} items")
    for (item <- items.values) {
      if (item.volume.milliliters == Int.MaxValue) {
        println(s"WARNING: ${item.name} has undefined volume")
      }
    }

    val itemGroups: Map[String, YamlObject.ItemGroup[YamlObject.ItemWithExtras]] = ymls("item group")
      .map(parse[YamlObject.ItemGroup[YamlObject.ItemWithExtras]])
      .groupBy(_.name).map {
        case (k, v) => assert(v.length == 1, s"More than one item group with name $k"); k -> v.head
      }

    def checkValid(referrer: String, name: String, table: Table[YamlObject.ItemWithExtras]): Unit = {
      table match {
        case Population.TableElement(item) =>
          if (!items.contains(item.item))
            println(s"Warning: $referrer '$name' referred to item '$item', which was not defined.")
        case Population.TableGroup(group) =>
          if (!itemGroups.contains(group))
            println(s"Warning: $referrer '$name' referred to item group '$group', which was not defined.")
        case Population.TableChoose(choose) =>
          choose.foreach { tce => checkValid(referrer, name, tce.subtable) }
        case Population.TableEach(each) =>
          each.foreach(checkValid(referrer, name, _))
        case Population.TableRepeat(_, repeat) =>
          checkValid(referrer, name, repeat)
        case Population.TableOptional(_, optional) =>
          checkValid(referrer, name, optional)
      }
    }

    itemGroups foreach { case (name, ig) => checkValid("item group", name, ig.choose) }

    val roomgens: Map[String, RoomGen] = ymls("roomgen")
      .map(parse[YamlObject.RoomGen])
      .groupBy(_.name).map { case (k: String, v: Seq[YamlObject.RoomGen]) =>
        assert(v.length == 1, s"More than one roomgen with name $k"); k -> v.head
      }
      .map { case (k, v) =>
        val algorithm = try {
          RoomGenAlgorithm.decoders(v.algorithm)
            .decodeJson(Json.fromJsonObject(v.options))
            .fold(
              ex => parseError(s"roomgen options for '$k'", Json.fromJsonObject(v.options), ex),
              identity
            )
        } catch {
          case e: Throwable =>
            throw new RuntimeException(s"Failed to parse roomgen: '$k'", e)
        }
        k -> RoomGen(algorithm = algorithm, minArea = v.minArea, maxArea = v.maxArea)
      }

    roomgens foreach { case (name, rg) =>
      rg.algorithm match {
        case WFC(parts, defs) =>
          defs.values.foreach { pd =>
            pd.items.foreach { items =>
              //checkValid("WFC roomgen", name, items)
            }
          }
        case _ =>
      }
    }

    val terrain: Map[String, Terrain] = ymls("terrain")
      .map(parse[Terrain])
      .groupBy(_.name).map {
        case (k, v) => assert(v.length == 1, s"More than one terrain with name $k"); k -> v.head
      }

    val display: DisplayData = {
      val displays = ymls("display")
      assert(displays.size == 1, "Must have exactly one display object")
      parse[DisplayData](displays.head)
    }

    Data(
      items,
      itemGroups,
      roomgens,
      terrain,
      display
    )
  }

  private def parseItems(items: Seq[Json], operations: Map[String, ItemOperation]): Map[String, ItemKind] = {
    val itemDefs = items.map(parse[YamlObject.ItemKind])
    val itemsById: Map[String, YamlObject.ItemKind] = itemDefs.groupBy(_.name).map {
      case (k, v) =>
        assert(v.size == 1, s"more than one item with the name '$k'")
        k -> v.head
    }
    val lazyMap = mutable.Map.empty[String, ItemKind]

    def itemForId(id: String): ItemKind = {
      lazyMap.getOrElseUpdate(
        id, {
          val i = itemsById.getOrElse(id, throw new NoSuchElementException(s"Item $id not found"))
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
              if (!itemsById.contains(p.`type`)) {
                throw new NoSuchElementException(s"Item '${i.name}' referenced part '${p.`type`}', but that item doesn't exist")
              }
              ItemPart(itemForId(p.`type`), p.count, operation)
            },
            volume = i.volume,
            display = i.display,
            behaviors = behaviorGenerators
          )
        }
      )
    }

    itemsById.map { case (k, _) => k -> itemForId(k) }
  }
}

