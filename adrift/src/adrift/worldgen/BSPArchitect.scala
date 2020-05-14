package adrift.worldgen

import java.awt.{Color, Dimension, Graphics}
import java.awt.event.{KeyEvent, KeyListener}

import adrift.GameState
import adrift.worldgen.NEATArchitect.RoomTypeId
import javax.swing.{JFrame, JPanel}

import scala.util.Random

object BSPArchitect {
  // TODO: make these parameters to the algorithm
  val cylinderCircumference = 360
  val cylinderLength = 270

  case class Rect(l: Int, t: Int, r: Int, b: Int) {
    require(r > l)
    require(b > t)

    def area: Int = (r - l) * (b - t)
    def width: Int = r - l
    def height: Int = b - t
  }

  def splitHorizontal(rect: Rect, spans: IterableOnce[(Int, Int)]): Iterator[Rect] =
    for ((l, r) <- spans) yield Rect(rect.l + l, rect.t, rect.l + r, rect.b)
  def splitVertical(rect: Rect, spans: IterableOnce[(Int, Int)]): Iterator[Rect] =
    for ((t, b) <- spans) yield Rect(rect.l, rect.t + t, rect.r, rect.t + b)

  def subdivideHorizontal(r: Rect, corridorWidth: Int, t: Double): Iterator[Rect] = {
    require(0 <= t && t <= 1, "t must be in [0,1]")
    require(corridorWidth >= 0, "corridor width must be non-negative")
    val corridorSpace = if (corridorWidth == 0) 1 else 1 + corridorWidth + 1 // with walls
    require(r.width >= 1 + corridorSpace + 1, "room too small to subdivide")
    val cut = 1 + ((r.width - (1 + corridorSpace + 1)) * t).round.toInt
    splitHorizontal(r, Seq((0, cut), (cut + corridorSpace - 1, r.width)))
  }

  def subdivideVertical(r: Rect, corridorWidth: Int, t: Double): Iterator[Rect] = {
    require(0 <= t && t <= 1, "t must be in [0,1]")
    require(corridorWidth >= 0, "corridor width must be non-negative")
    val corridorSpace = if (corridorWidth == 0) 1 else 1 + corridorWidth + 1 // with walls
    require(r.height >= 1 + corridorSpace + 1, "room too small to subdivide")
    val cut = 1 + ((r.height - (1 + corridorSpace + 1)) * t).round.toInt
    splitVertical(r, Seq((0, cut), (cut + corridorSpace - 1, r.height)))
  }

  def clamp01(d: Double) = math.max(0, math.min(1, d))
  def subdivideRecursive(r: Rect, corridorWidth: Int)(implicit random: Random): IterableOnce[Rect] = {
    // 1. Which way are we going to divide?
    val divideHorizontal = r.width > r.height || (r.width == r.height && random.nextBoolean())
    def splitValue: Double = clamp01(random.nextGaussian() * 0.2 + 0.5)
    val corridorsRequired = r.area > 1000
    val minCorridorWidth = if (corridorsRequired) 1 else 0
    if (divideHorizontal) {
      // Horizontal
      if (r.width < (1 + 1 + corridorWidth + 1 + 1) || r.area < 100) {
        // Can't be further subdivided.
        Seq(r)
      } else {
        subdivideHorizontal(r, corridorWidth, splitValue)
          .flatMap(r => subdivideRecursive(r, math.max(minCorridorWidth, corridorWidth - 1)))
      }
    } else {
      // Vertical
      if (r.height < (1 + 1 + corridorWidth + 1 + 1) || r.area < 100) {
        Seq(r)
      } else {
        subdivideVertical(r, corridorWidth, splitValue)
          .flatMap(r => subdivideRecursive(r, math.max(minCorridorWidth, corridorWidth - 1)))
      }
    }
  }

  def test() = subdivideRecursive(Rect(0, 0, cylinderCircumference, cylinderLength), 3)(new Random(42))

  case class Layout(
    bounds: Rect,
    rooms: Seq[Rect]
  )

  def generate()(implicit random: Random): Layout = {
    val bounds = Rect(0, 0, cylinderCircumference, cylinderLength)
    val rooms = subdivideRecursive(bounds, 3).iterator.to(Seq)
    Layout(bounds, rooms)
  }

  def main(args: Array[String]): Unit = {
    val frame = new JFrame("Adrift")
    frame.setDefaultCloseOperation(3)

    val rects = test().iterator.to(Seq)

    val colors = Seq(
      Color.BLUE,
      Color.CYAN,
      Color.DARK_GRAY,
      Color.GRAY,
      Color.GREEN,
      Color.LIGHT_GRAY,
      Color.MAGENTA,
      Color.ORANGE,
      Color.PINK,
      Color.RED,
      Color.YELLOW,
    )

    val panel = new JPanel() {
      override def paint(g: Graphics): Unit = {
        g.setColor(Color.BLACK)
        for (r <- rects) {
          g.drawRect(r.l * 8, r.t * 8, r.width * 8, r.height * 8)
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
