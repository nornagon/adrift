package adrift

import java.nio.file._

import adrift.display.glutil.{Image, Texture}
import adrift.display._
import adrift.worldgen.WorldGen

import scala.util.Random

object Main {
  lazy val font: Font = Font(Texture.fromImage(Image.fromFile("cp437_8x8.png")), 8, 8, 2)

  class LoadingScreen(win: GLFWWindow, font: Font) {
    val (widthInGlyphs, heightInGlyphs) = (80, 24)  // just for the loading screen.
    private var buffer = Seq.empty[String]
    def println(s: String): Unit = {
      buffer :++= GlyphRenderer.wrapString(widthInGlyphs, Integer.MAX_VALUE, s)
      render()
    }
    def updateLastLine(s: String): Unit = {
      buffer = buffer.init :+ s
      render()
    }
    def init(): Unit = {
      win.init()
      win.setSize(font.tileWidth * font.scaleFactor * widthInGlyphs, font.tileHeight * font.scaleFactor * heightInGlyphs)
      win.center()
      win.show()
      win.poll()
    }

    def render(): Unit = {
      win.render { g =>
        val gr = g.glyphs(font)
        for ((l, y) <- buffer.zipWithIndex) {
          gr.drawString(0, y, l)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val win = new GLFWWindow
    val loadingScreen = new LoadingScreen(win, font)
    loadingScreen.init()
    val load = args.contains("--load")
    val emptyWorld = args.contains("--empty-world")

    val dataPath = Paths.get("data")
    loadingScreen.println("Loading data...")
    val data = Data.parse(dataPath)

    val savePath = Paths.get("save.bson")
    val state =
      if (Files.exists(savePath) && load) {
        loadingScreen.println(s"Loading from $savePath...")
        val start = System.nanoTime()
        val json = Bson.decode(Files.newInputStream(savePath))
        println(f"Parse took ${(System.nanoTime() - start) / 1e6}%.1f ms")
        val start2 = System.nanoTime()
        val state = Serialization.load(data, json)
        println(f"Load took ${(System.nanoTime() - start2) / 1e6}%.1f ms")
        state
      } else {
        loadingScreen.println("Generating map...")
        loadingScreen.println("")
        def printProgress(t: Double): Unit = {
          val width = 30
          val numEqs = (t * width).round.toInt
          loadingScreen.updateLastLine(f"  [${"=" * numEqs}>${" " * (width - numEqs)}] ${t * 100}%.0f%%")
        }
        implicit val random: Random = new Random(12367)
        val gen = WorldGen(data)
        val state = if (emptyWorld) gen.generateEmptyWorld() else gen.generateWorld(reportProgress = printProgress)
        state
      }

    state.refresh()
    val display: Display = new GLFWDisplay(win, font)
    display.init()
    display.update(state)

    FileWatcher.onFileChanged(dataPath) { _ =>
      display.postAction(Action.ReloadData(Data.parse(dataPath)))
    }

    while (display.running) {
      val action = display.waitForAction
      state.receive(action)
      display.update(state)
    }
  }
}
