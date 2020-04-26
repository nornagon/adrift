package adrift

import java.awt.event.{KeyEvent, KeyListener}
import java.awt.{Dimension, Graphics}

import adrift.worldgen.NEATArchitect.RoomTypeId
import adrift.worldgen.{ForceDirectedPlacement, NEATArchitect, RoomType}
import javax.swing.{JFrame, JPanel}

import scala.util.Random

object ArchitectTest {
  def main(args: Array[String]): Unit = {
    var seed = 42
    implicit val random: Random = new Random(seed)
    val genome = NEATArchitect.runGenerations(NEATArchitect.newPopulation(40), 100).best
    println(genome.evaluations)
    val gLayout = genome.layout

    val fdp: ForceDirectedPlacement = NEATArchitect.initializeFdp(genome)(new Random(genome.layoutSeed))
    val alphaSequence = fdp.alphaSequence()

    import java.awt.Color

    val frame = new JFrame("Adrift")
    frame.setDefaultCloseOperation(3)

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

        def round(p: (Double, Double)): (Int, Int) = (
          p._1.round.toInt,
          p._2.round.toInt
        )

        def colorForRoomType(rtId: RoomTypeId): Color =
          colors(((rtId.value % colors.size) + colors.size) % colors.size)
        def colorForRoom(rId: NEATArchitect.HistoricalId): Color =
          colorForRoomType(genome.rooms.find(_.id == rId).get.roomType)

        for (p <- gLayout.roomGrid.indices) {
          gLayout.roomGrid(p) foreach { rId =>
            if (!gLayout.roomGrid(p._1 + 1, p._2).contains(rId) || (p._2 < gLayout.roomGrid.height - 1 && !gLayout.roomGrid(p._1, p._2 + 1).contains(rId)))
              g.setColor(colorForRoom(rId).darker())
            else
              g.setColor(colorForRoom(rId))
            g.fillRect(p._1, p._2, 1, 1)
          }
        }

        // draw the graph
        val centers = genome.rooms.map(_.id).zipWithIndex.toMap.view.mapValues(fdp.position)
        //val centers: Map[NEATArchitect.HistoricalId, (Double, Double)] = gLayout.roomCenters
        for (conn <- genome.connections) {
          val aPos = centers(conn.a)
          val bPos = centers(conn.b)

          val (x1, y1) = round(aPos)
          val (x2, y2) = round(bPos)

          if (math.abs(x2 - x1) < 1000 - math.abs(x2 - x1)) {
            g.setColor(new Color(0, 0, 0, 64))
            g.drawLine(x1, y1, x2, y2)
          } else {
            g.setColor(Color.PINK)
            g.drawLine(x1, y1, x2, y2)
          }
        }

        for ((rId, p) <- centers) {
          val (x, y) = round(p)
          g.setColor(new Color(0, 0, 0, 64))
          g.drawRect(x - 2, y - 2, 4, 4)
        }

        g.setColor(Color.WHITE)
        val legendWidth = 100
        val legendRight = 10
        val legendLeft = this.getWidth - legendWidth - legendRight
        val legendLineHeight = 12
        val legendTop = 10
        val legendSquareSize = legendLineHeight - 2

        g.fillRect(legendLeft, legendTop, legendWidth, legendLineHeight * RoomType.byId.size + 2)
        g.setColor(Color.BLACK)
        g.drawRect(legendLeft, legendTop, legendWidth, legendLineHeight * RoomType.byId.size + 2)
        for ((rtId, i) <- RoomType.byId.keys.zipWithIndex) {
          g.setColor(colorForRoomType(rtId))
          g.fillRect(legendLeft + 2, legendTop + i * legendLineHeight + 2, legendSquareSize, legendSquareSize)
          g.setColor(Color.BLACK)
          g.drawString(RoomType.byId(rtId).name, legendLeft + 2 + legendSquareSize + 2, legendTop + i * legendLineHeight + 2 + legendSquareSize - 1)
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
          if (alphaSequence.hasNext) {
            fdp.step(alphaSequence.next)
            frame.repaint()
          }
        }
      }

      override def keyReleased(e: KeyEvent): Unit = {}
    })
  }
}
