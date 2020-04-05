package adrift.display

import java.nio.file.{Files, Paths}

import adrift._
import org.lwjgl.glfw.GLFW

class WishScreen(display: GLFWDisplay, state: GameState) extends Screen {
  val setTemp = raw"settemp (-?[0-9.]+)".r
  val setGas = raw"setgas (\S+) (-?[0-9.]+)".r
  val item = raw"item (.+)".r
  var input = ""

  def longestCommonPrefix(strs: Traversable[String]): String = {
    if (strs.isEmpty) return ""
    val minLen = strs.view.map(_.length).min
    var low = 1
    var high = minLen
    while (low <= high) {
      val middle = (low + high) / 2
      if (isCommonPrefix(strs, middle)) low = middle + 1
      else high = middle - 1
    }
    strs.head.substring(0, (low + high) / 2)
  }

  private def isCommonPrefix(strs: Traversable[String], len: Int): Boolean = {
    val str1 = strs.head.substring(0, len)
    strs.tail.forall(_.startsWith(str1))
  }

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW.GLFW_PRESS || action == GLFW.GLFW_REPEAT) {
      key match {
        case GLFW.GLFW_KEY_BACKSPACE if input.length > 0 =>
          input = input.init
        case GLFW.GLFW_KEY_TAB =>
          input match {
            case item(partial) =>
              val matching = state.data.itemGroups.keySet union state.data.items.keySet filter (_.startsWith(partial))
              if (matching.nonEmpty)
                input = s"item ${longestCommonPrefix(matching)}"
            case _ =>
          }
        case GLFW.GLFW_KEY_ENTER =>
          input match {
            case "wtw" =>
              state.walkThroughWalls = !state.walkThroughWalls
            case "showtemp" =>
              state.showTempDebug = !state.showTempDebug
            case "showgas" =>
              state.showGasDebug = !state.showGasDebug
            case setGas("oxygen", pp) =>
              state.levels(state.player.levelId).gasComposition(state.player.x, state.player.y) = state.levels(state.player.levelId).gasComposition(state.player.x, state.player.y).copy(oxygen = pp.toFloat)
            case setTemp(t) =>
              state.levels(state.player.levelId).temperature(state.player.x, state.player.y) = t.toFloat
            case item(name) =>
              if (state.data.itemGroups.contains(name)) {
                state.sampleItem(state.data.itemGroups(name).choose).foreach(item => {
                  state.items.put(item, OnFloor(state.player))
                })
              } else if (state.data.items.contains(name)) {
                val item = state.data.items(name).generateItem()
                state.items.put(item, OnFloor(state.player))
              }
            case "save" =>
              val savePath = Paths.get("save.bson")
              Bson.encode(Serialization.save(state), Files.newOutputStream(savePath))
            case "reload" =>
              try {
                state.data = Data.parse(Paths.get("data"))
              } catch {
                case e: Throwable =>
                  e.printStackTrace()
                  state.putMessage(s"Couldn't reload: ${e.getMessage}")
              }
            case "colors" =>
              display.popScreen()
              display.pushScreen(new ColorTestScreen(display, state))
              return
            case _ =>
          }
          display.popScreen()
        case _ =>
      }
    }
  }

  override def char(char: Int): Unit = {
    input += char.toChar
  }

  override def render(renderer: GlyphRenderer): Unit = {
    renderer.frame(2, 2, 40, "Wish", Seq(""))
    renderer.drawString(3, 3, input, maxWidth = 38)
    renderer.drawString(3 + input.length, 3, "\u00db")
  }
}
