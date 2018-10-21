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
  type TableContext[T] = Map[String, Table[T]]

  sealed trait Table[T] {
    def sample()(implicit r: Random, ctx: TableContext[T]): Seq[T]
  }

  case class TableItem[T](item: T) extends Table[T] {
    def sample()(implicit r: Random, ctx: TableContext[T]): Seq[T] = Seq(item)
  }

  case class TableGroup[T](group: String) extends Table[T] {
    def sample()(implicit r: Random, ctx: TableContext[T]): Seq[T] = ctx(group).sample()
  }

  case class TableChooseEntry[T](subtable: Table[T], weight: Float = 1)

  case class TableChoose[T](choose: Seq[TableChooseEntry[T]]) extends Table[T] {
    override def sample()(implicit r: Random, ctx: TableContext[T]): Seq[T] =
      r.chooseFrom(choose)(_.weight).subtable.sample()
  }

  case class TableEach[T](each: Seq[Table[T]]) extends Table[T] {
    override def sample()(implicit r: Random, ctx: TableContext[T]): Seq[T] = each.flatMap(_.sample())
  }

  case class TableRepeat[T](count: CountSpec, repeat: Table[T]) extends Table[T] {
    override def sample()(implicit r: Random, ctx: TableContext[T]): Seq[T] =
      (1 to count.sample()) flatMap (_ => repeat.sample())
  }

  case class TableOptional[T](chance: Int, optional: Table[T]) extends Table[T] {
    override def sample()(implicit r: Random, ctx: TableContext[T]): Seq[T] =
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
    for {
      _ <- c.as[T].map(TableItem(_)).left
      _ <- c.as[Seq[TableChooseEntry[T]]].map(TableChoose(_)).left
      _ <- c.downField("group").up.as[TableGroup[T]].left
      _ <- c.downField("item").up.as[TableItem[T]].left
      _ <- c.downField("each").up.as[TableEach[T]].left
      _ <- c.downField("repeat").up.as[TableRepeat[T]].left
      _ <- c.downField("optional").up.as[TableOptional[T]].left
    } yield DecodingFailure(s"Unknown table type: ${c.focus}", c.history)
  }
}
