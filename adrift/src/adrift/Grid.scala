package adrift

import scala.collection.mutable

class Grid[T](val width: Int, val height: Int)(initial: => T) {
  val cells: mutable.Seq[T] = mutable.Seq.fill[T](width * height)(initial)
  def apply(x: Int, y: Int): T = {
    require(contains(x, y), s"Coordinate ($x, $y) was out of range $width x $height")
    cells(y * width + x)
  }
  def apply(pos: (Int, Int)): T = {
    apply(pos._1, pos._2)
  }
  def get(pos: (Int, Int)): Option[T] = get(pos._1, pos._2)
  def get(x: Int, y: Int): Option[T] = if (contains(x, y)) Some(apply(x, y)) else None
  def getOrElse(pos: (Int, Int), default: => T): T = get(pos).getOrElse(default)
  def update(pos: (Int, Int), c: T): Unit = {
    update(pos._1, pos._2, c)
  }
  def update(x: Int, y: Int, c: T): Unit = {
    require(contains(x, y), s"Coordinate ($x, $y) was out of range $width x $height")
    cells(y * width + x) = c
  }
  def contains(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
  def contains(pos: (Int, Int)): Boolean = contains(pos._1, pos._2)
  def indices: IndexedSeq[(Int, Int)] = for (y <- 0 until height; x <- 0 until width) yield (x, y)

  override def toString: String = {
    s"${this.getClass.getSimpleName}(" + (for (y <- 0 until height) yield {
      (for (x <- 0 until width) yield this(x, y).toString).mkString(", ")
    }).mkString("\n") + ")"
  }
}

class CylinderGrid[T](width: Int, height: Int)(initial: => T) extends Grid[T](width, height)(initial) {
  def normalizeX(x: Int): Int = ((x % width) + width) % width
  def normalize(xy: (Int, Int)): (Int, Int) =
    (normalizeX(xy._1), xy._2)

  override def apply(x: Int, y: Int): T = super.apply(normalizeX(x), y)
  override def get(x: Int, y: Int): Option[T] = super.get(normalizeX(x), y)
  override def update(x: Int, y: Int, c: T): Unit = super.update(normalizeX(x), y, c)
  override def contains(x: Int, y: Int): Boolean = y >= 0 && y < height
}