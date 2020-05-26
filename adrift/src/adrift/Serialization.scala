package adrift

import adrift.items.{Behavior, Item, ItemId}
import io.circe.{KeyDecoder, KeyEncoder}

import scala.collection.mutable
import scala.util.Random

object Serialization {
  import java.io._
  import java.util.Base64

  import io.circe.generic.semiauto._
  import io.circe.syntax._
  import io.circe.{Decoder, Encoder, HCursor, Json}

  case class GameStateSerialized(
    random: Random,
    levels: Map[LevelId, Level],
    memory: Map[LevelId, Grid[Option[(Char, Color, Color)]]],
    items: ItemDatabase,
    player: Location,
    currentTime: Int,
    messages: Seq[(String, Int)],
    bodyTemp: Double
  ) {
    def toGameState(data: Data): GameState = {
      val state = new GameState(data, random)
      state.levels ++= levels
      state.itemDb = items
      state.player = player
      state.bodyTemp = bodyTemp
      state.currentTime = currentTime
      state.messages = messages
      state.mapMemory = mutable.Map.empty ++ memory
      state.refresh()
      state
    }
  }
  object GameStateSerialized{
    def fromGameState(state: GameState): GameStateSerialized = GameStateSerialized(
      random = state.random,
      levels = state.levels.toMap,
      memory = state.mapMemory.toMap,
      items = state.itemDb,
      player = state.player,
      currentTime = state.currentTime,
      messages = state.messages,
      bodyTemp = state.bodyTemp,
    )
  }

  def encodeByJavaSerialization[T <: Serializable]: Encoder[T] = (a: T) => {
    val bs = new ByteArrayOutputStream()
    val s = new ObjectOutputStream(bs)
    s.writeObject(a)
    s.close()
    Json.fromString(Base64.getEncoder.encodeToString(bs.toByteArray))
  }

  def decodeByJavaSerialization[T <: Serializable]: Decoder[T] = (h: HCursor) => {
    for (str <- h.as[String]) yield {
      val bs = new ByteArrayInputStream(Base64.getDecoder.decode(str))
      val s = new ObjectInputStream(bs)
      s.readObject().asInstanceOf[T]
    }
  }

  implicit val encodeRandom: Encoder[Random] = encodeByJavaSerialization[Random]
  implicit val decodeRandom: Decoder[Random] = decodeByJavaSerialization[Random]

  implicit def encodeGrid[T: Encoder]: Encoder[Grid[T]] = (a: Grid[T]) =>
    Json.obj(
      "width" -> a.width.asJson,
      "height" -> a.height.asJson,
      "cells" -> a.cells.asJson
    )
  implicit def encodeCylinderGrid[T: Encoder]: Encoder[CylinderGrid[T]] = (a: CylinderGrid[T]) => encodeGrid[T](implicitly[Encoder[T]])(a)
  implicit def decodeGrid[T: Decoder]: Decoder[Grid[T]] = (c: HCursor) =>
    for {
      width <- c.get[Int]("width")
      height <- c.get[Int]("height")
      cells <- c.get[Vector[T]]("cells")
    } yield {
      val iter = cells.iterator
      new Grid[T](width, height)(iter.next())
    }
  implicit def decodeCylinderGrid[T: Decoder]: Decoder[CylinderGrid[T]] = (c: HCursor) =>
    for {
      width <- c.get[Int]("width")
      height <- c.get[Int]("height")
      cells <- c.get[Vector[T]]("cells")
    } yield {
      val iter = cells.iterator
      new CylinderGrid[T](width, height)(iter.next())
    }

  implicit val encodeTerrain: Encoder[Terrain] = (t: Terrain) => t.name.asJson
  implicit def decodeTerrain(implicit data: Data): Decoder[Terrain] = (c: HCursor) => c.as[String].map(data.terrain)

  implicit val encodeLevelIdAsKey: KeyEncoder[LevelId] = (t: LevelId) => t.id
  implicit val decodeLevelIdAsKey: KeyDecoder[LevelId] = (t: String) => Some(LevelId(t))
  implicit val encodeLevelId: Encoder[LevelId] = (t: LevelId) => t.id.asJson
  implicit def decodeLevelId: Decoder[LevelId] = (c: HCursor) => c.as[String].map(LevelId)

  implicit val encodeLocation: Encoder[Location] = deriveEncoder[Location]
  implicit val decodeLocation: Decoder[Location] = deriveDecoder[Location]

  implicit val encodeColor: Encoder[Color] = deriveEncoder[Color]
  implicit val decodeColor: Decoder[Color] = deriveDecoder[Color]

  implicit val encodeGasComposition: Encoder[GasComposition] = deriveEncoder
  implicit val decodeGasComposition: Decoder[GasComposition] = deriveDecoder

  implicit val encodeItemLocation: Encoder[ItemLocation] = {
    case OnFloor(Location(levelId, x, y)) =>
      Json.obj(
        "where" -> "OnFloor".asJson,
        "l" -> levelId.id.asJson,
        "x" -> x.asJson,
        "y" -> y.asJson
      )
    case InHands() =>
      Json.obj("where" -> "InHands".asJson)
    case Inside(other: Item) =>
      Json.obj(
        "where" -> "Inside".asJson,
        "container" -> other.id.asJson
      )
    case Worn() =>
      Json.obj("where" -> "Worn".asJson)
  }
  implicit def decodeItemLocation(implicit itemsById: Map[ItemId, Item]): Decoder[ItemLocation] = (h: HCursor) =>
    for {
      where <- h.get[String]("where")
      r <- where match {
        case "OnFloor" =>
          for {
            l <- h.get[String]("l")
            x <- h.get[Int]("x")
            y <- h.get[Int]("y")
          } yield OnFloor(Location(LevelId(l), x, y))
        case "InHands" =>
          Right(InHands())
        case "Inside" =>
          for (containerId <- h.get[Int]("container")) yield Inside(itemsById(new ItemId(containerId)))
        case "Worn" =>
          Right(Worn())
      }
    } yield r

  implicit val encodeBehavior: Encoder[Behavior] = Behavior.encodeBehavior
  implicit val decodeBehavior: Decoder[Behavior] = (c: HCursor) => {
    val behaviorKind = c.keys.get.head
    c.get[Behavior](behaviorKind)(Behavior.decoders(behaviorKind))
  }

  implicit val encodeItemId: Encoder[ItemId] = Encoder[Int].contramap(_.id)
  implicit val encodeItem: Encoder[Item] = (item: Item) =>
    Json.obj(
      "id" -> item.id.asJson,
      "kind" -> item.kind.name.asJson,
      "parts" -> item.parts.asJson,
      "behaviors" -> item.behaviors.asJson
    )
  implicit def decodeItem(implicit data: Data): Decoder[Item] = (c: HCursor) =>
    for {
      id <- c.get[Int]("id")
      kind <- c.get[String]("kind")
      parts <- c.get[Seq[Item]]("parts")
      behaviors <- c.get[mutable.Buffer[Behavior]]("behaviors")
    } yield {
      Item(data.items(kind), parts, behaviors, new ItemId(id))
    }

  implicit val encodeItems: Encoder[ItemDatabase] = (db: ItemDatabase) => {
    Json.fromValues(db.all.map(item => Json.obj(
      "loc" -> db.lookup(item).asJson,
      "item" -> item.asJson
    )))
  }
  implicit def decodeItems(implicit data: Data): Decoder[ItemDatabase] = (c: HCursor) => {
    implicit val locItemDecoder: Decoder[(Json, Item)] =
      Decoder.forProduct2("loc", "item")((loc: Json, item: Item) => (loc, item))
    val locItems = c.as[Vector[(Json, Item)]].fold(throw _, identity)
    implicit val itemsById: Map[ItemId, Item] =
      locItems.view.map { case (_, item) => item.id -> item }.to(Map)

    val db = new ItemDatabase
    for ((loc, item) <- locItems) {
      val decodedLoc: ItemLocation = loc.as[ItemLocation].right.get
      db.put(item, decodedLoc)
    }

    ItemId.reset(locItems.view.map(_._2.id.id).max)

    Right(db)
  }

  implicit def encodeLevel: Encoder[Level] = (a: Level) =>
    Json.obj(
      "terrain" -> a.terrain.asJson,
      "temperature" -> a.temperature.asJson,
      "gasComposition" -> a.gasComposition.asJson,
    )
  implicit def decodeLevel(implicit d: Data): Decoder[Level] = (c: HCursor) =>
    for {
      terrain <- c.get[CylinderGrid[Terrain]]("terrain")
      temperature <- c.get[CylinderGrid[Double]]("temperature")
      gasComposition <- c.get[CylinderGrid[GasComposition]]("gasComposition")
    } yield
      Level(terrain, temperature, gasComposition)

  implicit val encodeGameState: Encoder[GameState] =
    deriveEncoder[GameStateSerialized].contramap(GameStateSerialized.fromGameState _)
  implicit def decodeGameState(implicit data: Data): Decoder[GameState] =
    deriveDecoder[GameStateSerialized].map(_.toGameState(data))

  def save(state: GameState): Json = Encoder[GameState].apply(state)
  def load(implicit data: Data, json: Json): GameState = Decoder[GameState].decodeJson(json).fold(e => throw new RuntimeException(s"Error loading save", e), c => c)
}
