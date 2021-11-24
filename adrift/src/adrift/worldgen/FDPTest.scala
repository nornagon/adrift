package adrift.worldgen

import java.awt.event.{KeyEvent, KeyListener}
import java.awt.{Dimension, Graphics}

import javax.swing.{JFrame, JPanel}

import scala.util.Random

object FDPTest {
  def main(args: Array[String]): Unit = {
    import java.awt.Color
    var seed = 42
    implicit val random: Random = new Random(seed)

    val nodes = 40
    val links = (1 until nodes).flatMap { u =>
      Seq.fill(random.between(1, 3))((random.between(0, u), u))
    }
    def neighbors(u: Int) = links.flatMap {
      case (a, b) if a == u => Some(b)
      case (a, b) if b == u => Some(a)
      case _ => None
    }

    val fdp = new ForceDirectedPlacement(
      nodes,
      neighbors = neighbors,
      desiredEdgeLength = (_, _) => 30d,
      initialPosition = _ => (random.nextDouble()*1000, random.nextDouble()*1000),
    )

    val frame = new JFrame("Adrift")
    frame.setDefaultCloseOperation(3)

    val panel = new JPanel() {
      override def paint(g: Graphics): Unit = {
        val ps = fdp.positions

        g.setColor(Color.BLACK)
        for ((u, v) <- links) {
          val (ux, uy) = ps(u)
          val (vx, vy) = ps(v)
          g.drawLine(ux.round.toInt, uy.round.toInt, vx.round.toInt, vy.round.toInt)
        }
        for (u <- 0 until nodes) {
          import scala.language.implicitConversions
          implicit def round(x: Double): Int = x.round.toInt
          val (x, y) = ps(u)
          g.setColor(Color.BLACK)
          g.drawArc(x.round.toInt - 2, y.round.toInt - 2, 4, 4, 0, 360)
          val (vx, vy) = fdp.velocity(u)
          g.setColor(Color.RED)
          g.drawLine(x, y, x + vx, y + vy)
        }
      }
    }

    panel.setPreferredSize(new Dimension(1000, 1000))
    frame.getContentPane.add(panel)
    frame.pack()

    val alphaSequence = fdp.alphaSequence()
    frame.setVisible(true)
    frame.addKeyListener(new KeyListener {
      override def keyPressed(e: KeyEvent): Unit = {
        if (e.getKeyChar == ' ') {
          if (alphaSequence.hasNext)
            fdp.step(alphaSequence.next())
          frame.repaint()
        }
      }
      override def keyTyped(e: KeyEvent): Unit = {}
      override def keyReleased(e: KeyEvent): Unit = {}
    })
  }
}
