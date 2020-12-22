package adrift

import adrift.RandomImplicits._
import io.circe._

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
    c.as[Int].map(CountSpecExact)
      .orElse(c.as[String].flatMap {
        case diceRegex(numDice, numSides) => Right(CountSpecDice(numDice.toInt, numSides.toInt))
        case rangeRegex(low, high) => Right(CountSpecRange(low.toInt, high.toInt))
        case intRegex(num) => Right(CountSpecExact(num.toInt))
        case other => Left(DecodingFailure(s"Unparseable count spec: '$other'", c.history))
      })
  }

  final case class Chance(chance: Double)

  implicit val decodeChance: Decoder[Chance] = (c: HCursor) => {
    c.as[Double].map(Chance)
      .orElse(c.as[String].flatMap {
        case percentageRegex(percentage) => Right(Chance(percentage.toDouble / 100))
        case other => Left(DecodingFailure(s"Unparseable chance: '$other'", c.history))
      })
  }

  /**
    * A population table, for generating (groups of) things, e.g. the contents of a wardrobe.
    * <p>
    * YAML form looks like:
    * <pre>
    *   # a single item
    *   "foo"
    *
    *   # an equal choice between three items
    *   { choose: [foo, bar, baz] }
    *
    *   # an array is implicitly a choice, so this is the same as above
    *   [ foo, bar, baz ]
    *
    *   # weighted choices, foo 3/4 bar 1/4
    *   [ { item: foo, weight: 3 }, bar ]
    *
    *   # generate all subgroups instead of choosing one
    *   { each: [foo, bar, baz] }
    *
    *   # repeating things
    *   { repeat: foo, count: 3d4 }
    *
    *   # repeating a nested group
    *   {
    *     count: 2-3
    *     repeat: {
    *       choose:
    *         - foo
    *         - bar
    *         - baz
    *     }
    *   }
    *
    *   # optional things
    *   { optional: foo, chance: 75% }
    *
    *   # cross-reference to another table using the table context
    *   { group: name of group }
    *
    *   # composing multiple techniques
    *   each:
    *     - { optional: { repeat: foo, count: 1-2 }, chance: 50% }
    *     - choose:
    *       - quux
    *       - { group: bar, weight: 3 }
    *       - { repeat: baz, count: 2d3 }
    * </pre>
    */
  type TableContext[T] = PartialFunction[String, Table[T]]

  sealed trait Table[T] {
    def map[K](f: T => K): Table[K]

    def sample()(implicit r: Random, ctx: TableContext[T]): Seq[T]
  }

  case class TableElement[T](item: T) extends Table[T] {
    def sample()(implicit r: Random, ctx: TableContext[T]): Seq[T] = Seq(item)

    override def map[K](f: T => K): Table[K] = TableElement(f(item))
  }

  case class TableGroup[T](group: String) extends Table[T] {
    def sample()(implicit r: Random, ctx: TableContext[T]): Seq[T] = ctx(group).sample()

    override def map[K](f: T => K): Table[K] = TableGroup[K](group)
  }

  case class TableChooseEntry[T](subtable: Table[T], weight: Float = 1) {
    def map[K](f: T => K): TableChooseEntry[K] = TableChooseEntry(subtable.map(f), weight)
  }

  case class TableChoose[T](choose: Seq[TableChooseEntry[T]]) extends Table[T] {
    override def sample()(implicit r: Random, ctx: TableContext[T]): Seq[T] =
      r.chooseFrom(choose)(_.weight).subtable.sample()

    override def map[K](f: T => K): Table[K] = TableChoose(choose.map(_.map(f)))
  }

  case class TableEach[T](each: Seq[Table[T]]) extends Table[T] {
    override def sample()(implicit r: Random, ctx: TableContext[T]): Seq[T] = each.flatMap(_.sample())

    override def map[K](f: T => K): Table[K] = TableEach(each.map(_.map(f)))
  }

  case class TableRepeat[T](count: CountSpec, repeat: Table[T]) extends Table[T] {
    override def sample()(implicit r: Random, ctx: TableContext[T]): Seq[T] =
      (1 to count.sample()) flatMap (_ => repeat.sample())

    override def map[K](f: T => K): Table[K] = TableRepeat(count, repeat.map(f))
  }

  case class TableOptional[T](chance: Chance, optional: Table[T]) extends Table[T] {
    override def sample()(implicit r: Random, ctx: TableContext[T]): Seq[T] =
      if (r.nextDouble() < chance.chance) optional.sample() else Seq.empty

    override def map[K](f: T => K): Table[K] = TableOptional(chance, optional.map(f))
  }

  private val diceRegex = raw"""(\d+)d(\d+)""".r("numDice", "numSides")
  private val rangeRegex = raw"""(\d+)-(\d+)""".r("low", "high")
  private val intRegex = raw"""(\d+)""".r("num")

  private val percentageRegex = raw"""(\d*\.?\d+)%""".r("percentage")

  import io.circe.generic.semiauto._
  def derivedTableItemDecoder[T](implicit d: Decoder[T]): Decoder[TableElement[T]] = deriveDecoder
  implicit def decodeTableElement[T](implicit d: Decoder[T]): Decoder[TableElement[T]] = (c: HCursor) => {
    c.as[T].map(TableElement(_))
      .orElse(derivedTableItemDecoder[T].apply(c))
      .orElse(Left(DecodingFailure(s"Failed to decode table item: ${c.focus}", c.history)))
  }
  implicit def decodeTableGroup[T](implicit d: Decoder[T]): Decoder[TableGroup[T]] = deriveDecoder
  implicit def decodeTableEach[T](implicit d: Decoder[T]): Decoder[TableEach[T]] = deriveDecoder
  implicit def decodeTableRepeat[T](implicit d: Decoder[T]): Decoder[TableRepeat[T]] = deriveDecoder
  implicit def decodeTableOptional[T](implicit d: Decoder[T]): Decoder[TableOptional[T]] = deriveDecoder
  implicit def decodeTableChooseEntry[T](implicit d: Decoder[T]): Decoder[TableChooseEntry[T]] = (c: HCursor) => {
    for {
      subtable <- c.as[Table[T]]
      weight = c.get[Float]("weight").getOrElse(1f)
    } yield TableChooseEntry(subtable, weight)
  }
  implicit def decodeTableChoose[T](implicit d: Decoder[T]): Decoder[TableChoose[T]] = deriveDecoder
  implicit def decodeTable[T](implicit d: Decoder[T]): Decoder[Table[T]] = (c: HCursor) => {
    c.as[T].map(TableElement(_))
      .orElse(c.as[Seq[TableChooseEntry[T]]].map(TableChoose(_)))
      .orElse(c.downField("group").up.as[TableGroup[T]])
      .orElse(c.downField("item").up.as[TableElement[T]])
      .orElse(c.downField("each").up.as[TableEach[T]])
      .orElse(c.downField("choose").up.as[TableChoose[T]])
      .orElse(c.downField("repeat").up.as[TableRepeat[T]])
      .orElse(c.downField("optional").up.as[TableOptional[T]])
      .orElse(Left(DecodingFailure(s"Unknown table type: ${c.focus}", c.history)))
  }

  object serialization {
    import io.circe.generic.semiauto._
    import io.circe.syntax._
    import io.circe.{Encoder, Json}

    implicit def encodeCountSpec: Encoder[CountSpec] = {
      case CountSpecExact(i) => i.asJson
      case cs: CountSpecRange => cs.toString.asJson
      case cs: CountSpecDice => cs.toString.asJson
    }
    implicit def encodeChance: Encoder[Chance] = (c: Chance) => c.chance.asJson

    implicit def encodeTableChoose[T](implicit e: Encoder[T]): Encoder[TableChoose[T]] = deriveEncoder
    implicit def encodeTableEach[T](implicit e: Encoder[T]): Encoder[TableEach[T]] = deriveEncoder
    implicit def encodeTableRepeat[T](implicit e: Encoder[T]): Encoder[TableRepeat[T]] = deriveEncoder
    implicit def encodeTableGroup[T](implicit e: Encoder[T]): Encoder[TableGroup[T]] = deriveEncoder
    implicit def encodeTableOptional[T](implicit e: Encoder[T]): Encoder[TableOptional[T]] = deriveEncoder
    implicit def encodeTableItem[T](implicit e: Encoder[T]): Encoder[TableElement[T]] = deriveEncoder
    implicit def encodeTable[T](implicit e: Encoder[T]): Encoder[Table[T]] = {
      case t: TableChoose[T] => t.asJson
      case t: TableEach[T] => t.asJson
      case t: TableRepeat[T] => t.asJson
      case t: TableGroup[T] => t.asJson
      case t: TableOptional[T] => t.asJson
      case t: TableElement[T] => t.asJson
    }
    implicit def encodeTableChooseEntry[T](implicit e: Encoder[T]): Encoder[TableChooseEntry[T]] = (entry: TableChooseEntry[T]) =>
      entry.subtable.asJson.deepMerge(Json.obj("weight" -> entry.weight.asJson))
  }
}
