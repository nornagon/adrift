package adrift

case class Rect(l: Int, t: Int, r: Int, b: Int) {
  require(r > l, s"Rect must have r > l, but was $this")
  require(b > t, s"Rect must have b > t, but was $this")

  def area: Int = width * height
  def width: Int = r - l
  def height: Int = b - t
  def center: (Int, Int) = ((l + r) / 2, (t + b) / 2)

  def flip: Rect = Rect(t, l, b, r)

  def splitHorizontal(spans: IterableOnce[(Int, Int)]): Iterator[Rect] =
    spans.iterator.map { case (l, r) => Rect(this.l + l, this.t, this.l + r, this.b) }
  def splitVertical(spans: IterableOnce[(Int, Int)]): Iterator[Rect] =
    spans.iterator.map { case (t, b) => Rect(this.l, this.t + t, this.r, this.t + b) }
  def cutVertical(cuts: Iterable[Int]): Iterator[Rect] =
    splitVertical(cuts.iterator.sliding(2).map { x => (x(0), x(1)) })
  def cutHorizontal(cuts: Iterable[Int]): Iterator[Rect] =
    splitHorizontal(cuts.iterator.sliding(2).map { x => (x(0), x(1)) })
}

object Rect {
  def centeredAt(center: (Int, Int), width: Int, height: Int): Rect = {
    val (x, y) = center
    Rect(l = x - width / 2, r = x + width / 2, t = y - height / 2, b = y + height / 2)
  }
}
