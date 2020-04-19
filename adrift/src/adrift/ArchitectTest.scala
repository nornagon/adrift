package adrift

import java.awt.event.{KeyEvent, KeyListener}
import java.awt.{Dimension, Graphics}

import adrift.worldgen.NEATArchitect
import javax.swing.{JFrame, JPanel}

import scala.util.Random

object ArchitectTest {
  def main(args: Array[String]): Unit = {
    var seed = 42
    implicit val random: Random = new Random(seed)
    val genome = NEATArchitect.runGenerations(NEATArchitect.newPopulation(10), 100).best
    var n = 1
    var growthIterationLimit = Int.MaxValue
    var gLayout = NEATArchitect.layout(genome, n)

    import java.awt.Color

    val frame = new JFrame("Adrift")
    frame.setDefaultCloseOperation(3)

    val colors = Seq(
      Color.BLACK,
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
        val minX = gLayout.roomCenters.map(_._2._1).min
        val maxX = gLayout.roomCenters.map(_._2._1).max
        val minY = gLayout.roomCenters.map(_._2._2).min
        val maxY = gLayout.roomCenters.map(_._2._2).max
        val bigger = if (maxY - minY > maxX - minX) maxY - minY else maxX - minX
        val scale = 1000d / bigger

        g.setColor(Color.BLACK)

        def f(p: (Double, Double)): (Int, Int) = (
          ((p._1 - minX) * scale).round.toInt,
          ((p._2 - minY) * scale).round.toInt
        )

        def colorForRoom(rId: NEATArchitect.HistoricalId) = {
          colors(((rId.hashCode() % colors.size) + colors.size) % colors.size)
        }

        for (p <- gLayout.roomGrid.indices) {
          gLayout.roomGrid(p) foreach { rId =>
            g.setColor(colorForRoom(rId))
            g.fillRect(p._1, p._2, 1, 1)
          }
        }

/*
        for ((rId, rect) <- gLayout.roomRects) {
          g.setColor(colorForRoom(rId))
          val (l, t) = f(rect.l, rect.t)
          val (r, b) = f(rect.r, rect.b)
          g.drawRect(l, t, r - l, b - t)
        }
 */

        for (conn <- genome.connections) {
          val aPos = gLayout.roomCenters(conn.a)
          val bPos = gLayout.roomCenters(conn.b)

          val (x1, y1) = f(aPos)
          val (x2, y2) = f(bPos)

          if (math.abs(x2 - x1) < 1000 - math.abs(x2 - x1)) {
            g.setColor(Color.BLACK)
            g.drawLine(x1, y1, x2, y2)
          } else {
            g.setColor(Color.PINK)
            g.drawLine(x1, y1, x2, y2)
          }
        }

        for ((rId, p) <- gLayout.roomCenters) {
          val (x, y) = f(p)
          g.setColor(colorForRoom(rId))
          g.drawRect(x - 2, y - 2, 4, 4)
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
          n += 10
          growthIterationLimit = Int.MaxValue
          println(n)
          val begin = System.nanoTime()
          gLayout = NEATArchitect.layout(genome, n, growthIterationLimit)(new Random(seed))
          println(f"Took ${(System.nanoTime() - begin) / 1e6}%.2f ms")
          frame.repaint()
        }
        if (e.getKeyChar == 'i') {
          if (growthIterationLimit == Int.MaxValue)
            growthIterationLimit = 0
          else
            growthIterationLimit += 1000
          println(growthIterationLimit)
          val begin = System.nanoTime()
          gLayout = NEATArchitect.layout(genome, n, growthIterationLimit)(new Random(seed))
          println(f"Took ${(System.nanoTime() - begin) / 1e6}%.2f ms")
          frame.repaint()
        }
        if (e.getKeyChar == 'r') {
          seed = Random.nextInt()
          println(seed)
          growthIterationLimit = Int.MaxValue
          n = 1
          gLayout = NEATArchitect.layout(genome, n, growthIterationLimit)(new Random(seed))
          frame.repaint()
        }
      }

      override def keyReleased(e: KeyEvent): Unit = {}
    })
  }
}
