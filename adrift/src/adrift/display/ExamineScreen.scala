package adrift.display
import adrift.display.CP437.BoxDrawing
import adrift.{Color, GameState, Location, OnFloor}
import org.lwjgl.glfw.GLFW._

object DirectionKey {
  def unapply(key: Int): Option[(Int, Int)] =
      key match {
        case GLFW_KEY_LEFT | GLFW_KEY_H => Some((-1, 0))
        case GLFW_KEY_DOWN | GLFW_KEY_J => Some((0, 1))
        case GLFW_KEY_UP | GLFW_KEY_K => Some((0, -1))
        case GLFW_KEY_RIGHT | GLFW_KEY_L => Some((1, 0))
        case GLFW_KEY_Y => Some((-1, -1))
        case GLFW_KEY_U => Some((1, -1))
        case GLFW_KEY_B => Some((-1, 1))
        case GLFW_KEY_N => Some((1, 1))
        case _ => None
      }

}

class ExamineDirectionScreen(display: GLFWDisplay, state: GameState) extends Screen {
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_RELEASE) {
      key match {
        case DirectionKey((dx, dy)) =>
          display.popScreen()
          display.pushScreen(new ExamineScreen(display, state, state.player + (dx, dy)))
        case _ =>
      }
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    renderer.frame(renderer.bounds.center._1 - 8, 0, 16, null, Seq("Examine where?"))
  }
}

class ExamineScreen(display: GLFWDisplay, state: GameState, location: Location) extends Screen {
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {

  }

  override def render(renderer: GlyphRenderer): Unit = {
    import GlyphRenderer.{CS, Ann, wrapCS}
    val width = 24
    val Some((sx, sy)) = display.worldToScreen(state)(location.xy)
    val (char, fg, bg) = Appearance.charAtPosition(state, location)
    val lightGreen = Color.fromBytes(217, 255, 102)
    val disabledGreen = Color.fromBytes(77, 102, 0)
    val selectedGreen = Color.fromBytes(0, 140, 0)
    val darkGreen = Color.fromBytes(32, 64, 0)
    renderer.drawChar(sx, sy, char, fg, bg = darkGreen)
    renderer.drawChar(sx + 1, sy, BoxDrawing.L_R_, fg = lightGreen)
    var nextY = 0
    def sprintln(s: String, fg: Color = lightGreen, bg: Color = darkGreen): Unit =
      sprintlnColored(CS(s, Seq(Ann(0, s.length, fg))), bg = bg)
    def sprintlnColored(ss: CS, defaultFg: Color = lightGreen, bg: Color = darkGreen): Unit = {
      val left = sx + 2
      val top = nextY + sy
      var x = 0
      val y = 0
      renderer.drawChar(left + x, top + y, 0xdd, fg = lightGreen, bg = bg)
      x += 1

      for ((s, anns) <- ss.parts) {
        val fg = anns.lastOption.map(_.fg).getOrElse(defaultFg)
        val maxWidth = width - 2 - x
        renderer.drawString(left + x, top + y, s, fg = fg, bg = bg, maxWidth = maxWidth)
        x += math.min(maxWidth, s.length)
      }

      while (x < width - 1) {
        renderer.drawChar(left + x, top + y, ' ', bg = bg)
        x += 1
      }

      renderer.drawChar(left + x, top + y, 0xde, fg = lightGreen, bg = bg)

      nextY += 1
    }
    val items = state.items.lookup(OnFloor(location))
    for { y <- items.indices } {
      val bg = if (y == 0) selectedGreen else darkGreen
      sprintln(state.itemDisplayName(items(y)), bg = bg)
    }
    sprintln("")
    val actions = Seq(
      "e" -> CS("examine", Seq(Ann(0, 1, lightGreen))),
      "o" -> CS("open", Seq(Ann(0, 1, lightGreen))),
      "d" -> CS("diagnose", Seq(Ann(0, 1, lightGreen))),
      "r" -> CS("remove", Seq(Ann(0, 1, lightGreen))),
    )
    val actionStr = wrapCS(actions.map(_._2).reduce(_ + CS(" ", Seq.empty) + _), width - 2)
    actionStr.foreach(sprintlnColored(_, defaultFg = disabledGreen))
  }
}
