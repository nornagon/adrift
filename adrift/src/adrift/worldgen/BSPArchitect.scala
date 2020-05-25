package adrift.worldgen

import java.awt.event.{KeyEvent, KeyListener}
import java.awt.{Color, Dimension, Graphics}

import javax.swing.{JFrame, JPanel}

import scala.util.Random

object BSPArchitect {
  case class Rect(l: Int, t: Int, r: Int, b: Int) {
    require(r > l, s"Rect must have r > l, but was $this")
    require(b > t, s"Rect must have b > t, but was $this")

    def area: Int = (r - l) * (b - t)
    def width: Int = r - l
    def height: Int = b - t

    def flip: Rect = Rect(t, l, b, r)
  }

  def splitHorizontal(rect: Rect, spans: IterableOnce[(Int, Int)]): Iterator[Rect] =
    for ((l, r) <- spans) yield Rect(rect.l + l, rect.t, rect.l + r, rect.b)

  def subdivideHorizontal(r: Rect, corridorWidth: Int, t: Double, margin: Int = 1): Iterator[Rect] = {
    require(0 <= t && t <= 1, "t must be in [0,1]")
    require(corridorWidth >= 0, "corridor width must be non-negative")
    val corridorSpace = if (corridorWidth == 0) 1 else 1 + corridorWidth + 1 // with walls
    require(r.width >= margin + corridorSpace + margin, "room too small to subdivide")
    val cut = margin + ((r.width - (margin + corridorSpace + margin)) * t).round.toInt
    splitHorizontal(r, Seq((0, cut), (cut + corridorSpace - 1, r.width)))
  }

  def subdivideVertical(r: Rect, corridorWidth: Int, t: Double, margin: Int = 1): Iterator[Rect] =
    subdivideHorizontal(r.flip, corridorWidth, t, margin).map(_.flip)

  def clamp01(d: Double) = math.max(0, math.min(1, d))
  def subdivideRecursive(r: Rect, corridorWidth: Int)(implicit random: Random): IterableOnce[Rect] = {
    // 1. Which way are we going to divide?
    val divideHorizontal = r.width > r.height || (r.width == r.height && random.nextBoolean())
    def splitValue: Double = clamp01(random.nextGaussian() * 0.2 + 0.5)
    val corridorsRequired = r.area > 1000
    val minCorridorWidth = if (corridorsRequired) 1 else 0
    val childCorridorWidth = math.max(minCorridorWidth, corridorWidth - 1)

    val minRoomDimension = 5
    val corridorSpace = if (corridorWidth == 0) 1 else 1 + corridorWidth + 1 // with walls
    val minDimension = minRoomDimension + corridorSpace + minRoomDimension

    if (divideHorizontal) {
      // Horizontal
      if (r.width < minDimension || r.area < 100) {
        // Can't be further subdivided.
        Seq(r)
      } else {
        subdivideHorizontal(r, corridorWidth, splitValue, minRoomDimension)
          .flatMap(r => subdivideRecursive(r, childCorridorWidth))
      }
    } else {
      // Vertical
      if (r.height < minDimension || r.area < 100) {
        Seq(r)
      } else {
        subdivideVertical(r, corridorWidth, splitValue, minRoomDimension)
          .flatMap(r => subdivideRecursive(r, childCorridorWidth))
      }
    }
  }

  case class Layout(
    bounds: Rect,
    rooms: Seq[Rect]
  )

  def generate(circumference: Int, length: Int)(implicit random: Random): Layout = {
    val bounds = Rect(0, 0, circumference, length)
    val numVerticalCorridors = random.between(1, 4)
    val initialCuts = Seq(-1) ++ Seq.tabulate(numVerticalCorridors)(i => circumference * i / numVerticalCorridors).tail
    val initialRooms = splitHorizontal(bounds, (initialCuts :+ circumference).sliding(2).map(s => (s(0) + 2, s(1) - 2)))
    val rooms = initialRooms.flatMap(r => subdivideRecursive(r, 3)).iterator.to(Seq)
    Layout(bounds, rooms)
  }

  def main(args: Array[String]): Unit = {
    val frame = new JFrame("Adrift")
    frame.setDefaultCloseOperation(3)

    val rects = generate(360, 270)(new Random(42)).rooms

    val panel = new JPanel() {
      override def paint(g: Graphics): Unit = {
        g.setColor(Color.WHITE)
        g.fillRect(0, 0, getWidth, getHeight)
        g.setColor(Color.BLACK)
        g.translate(10, 10)
        for (r <- rects) {
          g.drawRect(r.l * 4, r.t * 4, r.width * 4, r.height * 4)
        }
      }
    }

    panel.setPreferredSize(new Dimension(1000, 1000))
    frame.getContentPane.add(panel)
    frame.pack()

    frame.setVisible(true)
    frame.addKeyListener(new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = {}

      override def keyPressed(e: KeyEvent): Unit = {
        if (e.getKeyChar == ' ') {
        }
      }

      override def keyReleased(e: KeyEvent): Unit = {}
    })
  }
}
