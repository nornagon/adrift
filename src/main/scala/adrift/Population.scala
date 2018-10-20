package adrift

import adrift.RandomImplicits._
import io.circe.{Decoder, DecodingFailure, HCursor, Json}

import scala.util.Random

object Population {

  /** A specification for how to generate random positive integers. */
  sealed trait CountSpec { def sample()(implicit r: Random): Int }

  case class CountSpecExact(i: Int) extends CountSpec {
    override def sample()(implicit r: Random): Int = i

    override def toString: String = i.toString
  }

  case class CountSpecRange(low: Int, high: Int) extends CountSpec {
    override def sample()(implicit r: Random): Int = r.between(low, high + 1)
    override def toString: String = s"$low-$high"
  }

  case class CountSpecDice(numDice: Int, numSides: Int) extends CountSpec {
    override def sample()(implicit r: Random): Int = {
      var x = 0
      for (_ <- 1 to numDice) x += 1 + r.nextInt(numSides)
      x
    }

    override def toString: String = s"${numDice}d$numSides"
  }

  // TODO: poisson?

  implicit val decodeCountSpec: Decoder[CountSpec] = (c: HCursor) => {
    for {
      _ <- c.as[Int].map(CountSpecExact).left
      err <- c.as[String].flatMap {
        case diceRegex(numDice, numSides) => Right(CountSpecDice(numDice.toInt, numSides.toInt))
        case rangeRegex(low, high) => Right(CountSpecRange(low.toInt, high.toInt))
        case intRegex(num) => Right(CountSpecExact(num.toInt))
        case other => Left(DecodingFailure(s"Unparseable count spec: '$other'", c.history))
      }.left
    } yield err
  }

  /**
    * A population table, for generating (groups of) things, e.g. the contents of a wardrobe.
    * <p>
    * YAML form looks like:
    * <pre>
    *   an equal choice between three items:
    *     - foo
    *     - bar
    *     - baz
    *
    *   cross-references:
    *     - { group: a single item }
    *
    *   a weighted choice, foo 3/4 bar 1/4:
    *     - { item: foo, weight: 3 }
    *     - bar
    *
    *   a single item: foo
    *
    *   repeating things:
    *     - { repeat: foo, count: 3d4 }
    *     - count: 2-3
    *       repeat:
    *         - foo
    *         - bar
    *         - baz
    *
    *   optional things:
    *     - { optional: foo, chance: 75 }
    *
    *   composition:
    *     each:
    *       - { optional: { repeat: foo, count: 1-2 }, chance: 50 }
    *       - choose:
    *         - foo
    *         - group: bar
    *         - { repeat: baz, count: 2d3 }
    * </pre>
    */
  sealed trait Table[T] {
    def sample()(implicit r: Random): Seq[T]
  }

  case class TableItem[T](item: T) extends Table[T] {
    def sample()(implicit r: Random): Seq[T] = Seq(item)
  }

  case class TableChooseEntry[T](subtable: Table[T], weight: Float = 1)

  case class TableChoose[T](choose: Seq[TableChooseEntry[T]]) extends Table[T] {
    override def sample()(implicit r: Random): Seq[T] =
      r.chooseFrom(choose)(_.weight).subtable.sample()
  }

  case class TableEach[T](each: Seq[Table[T]]) extends Table[T] {
    override def sample()(implicit r: Random): Seq[T] = each.flatMap(_.sample())
  }

  case class TableRepeat[T](count: CountSpec, repeat: Table[T]) extends Table[T] {
    override def sample()(implicit r: Random): Seq[T] =
      (1 to count.sample()) flatMap (_ => repeat.sample())
  }

  case class TableOptional[T](chance: Int, optional: Table[T]) extends Table[T] {
    override def sample()(implicit r: Random): Seq[T] =
      if (r.nextDouble() * 100 < chance) optional.sample() else Seq.empty
  }

  private val diceRegex = raw"""(\d+)d(\d+)""".r("numDice", "numSides")
  private val rangeRegex = raw"""(\d+)-(\d+)""".r("low", "high")
  private val intRegex = raw"""(\d+)""".r("num")

  import io.circe.generic.semiauto._
  def derivedTableItemDecoder[T](implicit d: Decoder[T]): Decoder[TableItem[T]] = deriveDecoder
  implicit def decodeTableItem[T](implicit d: Decoder[T]): Decoder[TableItem[T]] = (c: HCursor) => {
    for {
      _ <- derivedTableItemDecoder[T].apply(c).left
      _ <- c.as[T].map(TableItem(_)).left
    } yield DecodingFailure(s"Failed to decode table item: ${c.focus}", c.history)
  }
  type TableContext = Map[String, Json]
  implicit def decodeTableEach[T](implicit d: Decoder[T], ctx: TableContext): Decoder[TableEach[T]] = deriveDecoder
  implicit def decodeTableRepeat[T](implicit d: Decoder[T], ctx: TableContext): Decoder[TableRepeat[T]] = deriveDecoder
  implicit def decodeTableOptional[T](implicit d: Decoder[T], ctx: TableContext): Decoder[TableOptional[T]] = deriveDecoder
  implicit def decodeTable[T](implicit d: Decoder[T], ctx: TableContext): Decoder[Table[T]] = (c: HCursor) => {
    for {
      _ <- c.as[T].map(TableItem(_)).left
      _ <- c.as[Seq[TableChooseEntry[T]]].map(TableChoose(_)).left
      _ <- c.get[String]("group").flatMap(gid => ctx(gid).as[Table[T]]).left
      _ <- c.downField("item").up.as[TableItem[T]].left
      _ <- c.downField("each").up.as[TableEach[T]].left
      _ <- c.downField("repeat").up.as[TableRepeat[T]].left
      _ <- c.downField("optional").up.as[TableOptional[T]].left
    } yield DecodingFailure(s"Unknown table type: ${c.focus}", c.history)
  }
  implicit def decodeTableChooseEntry[T](implicit d: Decoder[T], ctx: TableContext): Decoder[TableChooseEntry[T]] = (c: HCursor) => {
    for {
      subtable <- c.as[Table[T]]
      weight = c.get[Float]("weight").getOrElse(1f)
    } yield TableChooseEntry(subtable, weight)
  }
  implicit def decodeTableChoose[T](implicit d: Decoder[T], ctx: TableContext): Decoder[TableChoose[T]] = deriveDecoder

  /*
  def readTables[T](reader: Reader)(implicit decoder: Decoder[T]): Map[String, Table[T]] = {
    import io.circe.{Json, yaml}
    val json = yaml.parser.parse(reader).fold(throw _, identity)
    implicit val groupJsonById: TableContext = json.as[Map[String, Json]].fold(throw _, identity)

    groupJsonById.map {
      case (k, v) => k -> v.as[Table[T]].fold(
        e => throw new RuntimeException(s"Couldn't decode group $k", e),
        identity)
    }
  }

  sealed trait ItemSpec
  object ItemSpec {
    case class Blueprint(blueprintId: String) extends ItemSpec
    case class BlueprintWithContents(container: String, contents: Table[ItemSpec]) extends ItemSpec
  }

  implicit def decodeItemSpec(implicit ctx: TableContext): Decoder[ItemSpec] = (c: HCursor) => {
    for {
      _ <- c.as[String].map(ItemSpec.Blueprint).left
      _ <- c.as[ItemSpec.BlueprintWithContents](deriveDecoder).left
    } yield DecodingFailure(s"Unknown item spec: ${c.focus}", c.history)
  }

  def readItemSpecTables(reader: Reader): Map[String, Table[ItemSpec]] = {
    import io.circe.{Json, yaml}
    val json = yaml.parser.parse(reader).fold(throw _, identity)
    implicit val groupJsonById: TableContext = json.as[Map[String, Json]].fold(throw _, identity)
    groupJsonById.map {
      case (k, v) => k -> v.as[Table[ItemSpec]].fold(
        e => throw new RuntimeException(s"Couldn't decode group $k", e),
        identity)
    }
  }
  */
}
