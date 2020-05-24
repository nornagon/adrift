package adrift

import java.nio.file._

import adrift.display.glutil.{Image, Texture}
import adrift.display.{Display, Font, GLFWDisplay, GLFWWindow}
import adrift.worldgen.WorldGen

import scala.util.Random

object Main {
  lazy val font: Font = Font(Texture.fromImage(Image.fromFile("cp437_8x8.png")), 8, 8, 2)

  def main(args: Array[String]): Unit = {
    val win = new GLFWWindow
    win.init()
    val (widthInGlyphs, heightInGlyphs) = (80, 24)
    win.setSize(font.tileWidth * font.scaleFactor * widthInGlyphs, font.tileHeight * font.scaleFactor * heightInGlyphs)
    win.center()
    win.show()
    win.poll()
    val load = args.contains("--load")

    val dataPath = Paths.get("data")
    win.render { g =>
      val gr = g.glyphs(font)
      gr.drawString(0, 0, "Loading data...")
    }.poll()
    val data = Data.parse(dataPath)

    val savePath = Paths.get("save.bson")
    val state =
      if (Files.exists(savePath) && load) {
        win.render { g =>
          val gr = g.glyphs(font)
          gr.drawString(0, 0, s"Loading from $savePath...")
        }.poll()
        val start = System.nanoTime()
        val json = Bson.decode(Files.newInputStream(savePath))
        println(f"Parse took ${(System.nanoTime() - start) / 1e6}%.1f ms")
        val start2 = System.nanoTime()
        val state = Serialization.load(data, json)
        println(f"Load took ${(System.nanoTime() - start2) / 1e6}%.1f ms")
        state
      } else {
        win.render { g =>
          val gr = g.glyphs(font)
          gr.drawString(0, 0, "Generating map...")
        }.poll()
        implicit val random: Random = new Random(12367)
        val gen = WorldGen(data)
        val state = gen.generateWorld
        state.refresh()
        state
      }

    state.recalculateFOV()
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
