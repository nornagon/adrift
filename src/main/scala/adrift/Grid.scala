package adrift

import scala.collection.mutable

class Grid[T](val width: Int, val height: Int)(initial: T) {
  private val cells: mutable.Seq[T] = mutable.Seq.fill[T](width * height)(initial)
  def apply(x: Int, y: Int): T = {
    require(contains(x, y), s"Coordinate ($x, $y) was out of range $width x $height")
    cells(y * width + x)
  }
  def update(x: Int, y: Int, c: T): Unit = {
    require(contains(x, y), s"Coordinate ($x, $y) was out of range $width x $height")
    cells(y * width + x) = c
  }
  def contains(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
  def indices: IndexedSeq[(Int, Int)] = for (y <- 0 until height; x <- 0 until width) yield (x, y)
}

